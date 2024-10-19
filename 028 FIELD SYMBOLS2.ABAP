* Declaração dos Field-Symbols
*-----------------------------------
FIELD-SYMBOLS: <fs_teste> TYPE ANY.

* Variável Qualquer
*-----------------------------------
DATA: v_matnr TYPE mara-matnr.

* Cria a referência da área de memória no Field-Symbol. Note que
* é necessário colocar o nome da variável entre ( ), além de o nome
* estar em letras maíusculas.
ASSIGN ('V_MATNR') TO <fs_teste>.

IF  IS ASSIGNED.
 <fs_teste>  = 'MATERIALZUMBI'.
ENDIF.

WRITE v_matnr.

* A vantagem principal é que você pode usar um mesmo field-symbol para alterar variáveis diferentes com a mesma linha de código. Como no exemplo abaixo:
* Sempre que você for atribuir valores a um Field-Symbol verifique antes se o mesmo foi realmente referenciado, através da consistência IS ASSIGNED. 
* Se você tentar usar um Field Symbols que não foi corretamente referenciado, o programa irá gerar um DUMP! Você também pode desfazer a referência através do comando UNASSIGN <fs>.
* Mas qual a vantagem disso tudo? Elementar meu caro leitor: usar o mesmo field-symbol para alterar valores de campos diferentes! 😀

REPORT zombie_assign.

* Declaração dos Field-Symbols
*-----------------------------------
FIELD-SYMBOLS: <fs_teste> TYPE ANY.

* Note que cada variável é de um tipo diferente!
*-----------------------------------
DATA: v_matnr TYPE mara-matnr,
      v_mtart TYPE mara-mtart,
      v_labst TYPE mard-labst.

* Cria a referência da área de memória no Field-Symbol. Note que
* é necessário colocar o nome da variável entre ( ), além de o nome
* estar em letras maíusculas.
ASSIGN ('V_MATNR') TO <fs_teste>.

IF <fs_teste> IS ASSIGNED.
  <fs_teste> = 'MATERIALZUMBI'.
ENDIF.

WRITE <fs_teste>.

ASSIGN ('V_MTART') TO <fs_teste>.

IF <fs_teste> IS ASSIGNED.
  <fs_teste> = 'FERT'.
ENDIF.

WRITE <fs_teste>.

ASSIGN ('V_LABST') TO <fs_teste>.

IF <fs_teste> IS ASSIGNED.
  <fs_teste> = '1234'.
ENDIF.

WRITE <fs_teste>.

* O ASSIGN ainda tem mais coisas que podem ajudá-lo em diversos momentos. Vamos lá:
* ASSIGN COMPONENT OF STRUCTURE: Ele permite que você associe num Field-Symbol um campo qualquer de uma estrutura. E você nem precisa saber o nome exato do campo, só a posição dele na sequência de declaração da estrutura. vejamos:

REPORT zombie_assign.

* Declaração dos Field-Symbols
*-----------------------------------
FIELD-SYMBOLS: <fs_wa> TYPE ANY.

* Work Area
*-----------------------------------
DATA: wa_mara TYPE mara.

* Assign de estrutura com o nome do campo da estrutura
ASSIGN COMPONENT 'MATNR' OF STRUCTURE wa_mara
                                   TO <fs_wa>.

IF <fs_wa> IS ASSIGNED.
  <fs_wa> = 'MATERIALZUMBI'.
ENDIF.

WRITE wa_mara-matnr.

* O campo 10 é o campo MTART
ASSIGN COMPONENT 10 OF STRUCTURE wa_mara
                              TO <fs_wa>.

IF <fs_wa> IS ASSIGNED.
  <fs_wa> = 'FERT'.
ENDIF.

WRITE <fs_wa>.

* ASSIGN CASTING: Ele faz a mesma referência dos outros, mas o field-symbol mostra o valor do tipo que você usou no casting. Ou seja, você pode associar uma variável char à um field-symbol que irá mostrar o seu valor em hexadecimal.
* Isso é útil para achar e manipular caracteres especiais em strings, como quebras de linha e tabs indesejados (aqueles que aparecem como # no debug).

REPORT zombie_assign.

* Declaração dos Field-Symbols
*-----------------------------------
FIELD-SYMBOLS: <fs_x> TYPE ANY.

* Variável CHAR
*-----------------------------------
DATA: v_char TYPE char1.

* Referenciando o valor de V_CHAR trocando o tipo
ASSIGN ('V_CHAR') TO <fs_x> CASTING TYPE x.

* Se movermos AB para V_CHAR..
v_char = 'A'.

*... o WRITE do Field-Symbol escreverá 41, que é o código em hexa do
* A  maiusculo :)
WRITE <fs_x>.
* – ASSIGN de variáveis de outros programas: Você pode associar a memória não só do seu programa, mas de qualquer outro programa carregado na memória da execução. Isso é muito usado em EXITs, BADIs e Enhancement Points, para a leitura de dados que nem sempre estão disponíveis na interface do FORM, Função ou método:

* Um exemplo simples de cada capturar valores de outros programas carregados na memória.
* Neste caso, estou capturando o valor de uma tabela que está declarada no
* programa SAPMV45A e associando a referência no field-symbol.
ASSIGN ('(SAPMV45A)XVBAP') TO <fs_tabela>.

* Note que você pode usar as dicas do tópico do Novo Debugger
* para navegar entre a pilha de programas carregados e verificar
*as variáveis que estão na memória.
* - LOOP AsSIGNING / READ ASSIGNING: Você pode ainda associar uma estrutura durante um loop ou um read, economizando uma work area e alterando os valores direto na tabela. Eu uso muito isso para alterar valores na tabela em que estou dando um Loop:

REPORT zombie_assign.

* Nesta declaração é necessário definir o tipo exato da Work Area para
* o Field-Symbol, caso contrário o comando <fs_mara>-matnr não irá
* ativar...
FIELD-SYMBOLS: <fs_mara> TYPE mara.

* Tabela
DATA: t_mara TYPE TABLE OF mara.

* Linha vazia, só para mostrar o Loop
APPEND INITIAL LINE TO t_mara.

LOOP AT t_mara ASSIGNING <fs_mara>.
* Associando o valor direto na linha
  <fs_mara>-matnr = 'MATERIALZUMBI'.
ENDLOOP.

* Analise a tabela. Sem MODIFY nem nada, os valores da linha foram
* alterados :) Você pode usar isto com o o READ TABLE também!
BREAK-POINT.
