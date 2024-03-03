*"------------------------------------------------------------------------------------------------------------
*"---------------------------------------READ TABLE-TUTORIAL--------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
*"RAFAEL-ALBUQUERQUE------------------------------------------------------------------------------------------
*"02-03-2024--------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
REPORT ZTEST2.

*" Transparent Table - ZPAYROLL
*"------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
*"
*" [FIELD]     [KEY]   [DATA ELEMENT]    [DATA TYPE]    [LENGTH]    [SHORT DESCRIPTION]
*" MANDT         X         MANDT            CLNT           3               Client
*" ID            X         ZID              INT1           3               Id Employee
*" BRUTESAL                ZBRUTESAL        DEC            6               Brute Salary
*" LIQSAL                  ZLIQSAL          DEC            6               Liquid Salary
*" HEALTHPLAN              ZHEALTHPLAN      CHAR           1               Health Plan
*" SINDICAL                ZSINDICAL        CHAR           1               Sindical Services
*" TRANSPORT               ZTRANSPORT       CHAR           1               Ticket Transport
*" EXTRAHOUR               ZEXTRAHOUR       INT1           3               Extra Hour
*" DEDUCTIONS              ZDEDUCTIONS      DEC            6               Deductions
*" SECSOCIAL               ZSECOSCIAL       DEC            6               Security Social
*" IRS                     ZIRS             DEC            6               IRS
*" BONUS                   ZBONUS           DEC            6               Bonus
*" SUBHOLYDAYS             ZSUBHOLYDAYS     DEC            1               Holydays
*" NOPAYLEAVE              ZNOPAYLEAVE      INT1           3               No Pay Leave
*" WORKSHIFT               ZWORKSHIFT       CHAR           5               Work Shift
*"
*"------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------

DATA: LT_TABLE TYPE TABLE OF ZPAYROLL, "DECLARACAO DE TABELA INTERNA."
      LS_STRUCT TYPE ZPAYROLL,         "DECLARACAO DE ESTRUTURA."
      LV_INDEX TYPE I.                 "DECLARACAO DE VARIAVEL NUMERICA PARA RECEBER O VALOR DE UM ÍNDICE."

LV_INDEX = 2. "VALOR DO ÍNDICE RECEBE 2."

SELECT * FROM ZPAYROLL INTO TABLE LT_TABLE. "PREENCHENDO TABELA INTERNA."

*"------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
"[LER ENTRADA POR UMA CHAVE]"
READ TABLE LT_TABLE INTO LS_STRUCT WITH KEY ID = 2.    "LER A LINHA DA TABELA INTERNA ONDE O ID É 1."

IF SY-SUBRC = 0.                                        "SE A INSTRUÇÃO FOR BEM SUCEDIDA...
  "WRITE LS_STRUCT-DEDUCTIONS.                           "EXIBE A MENSAGEM.
ELSE.                                                   "DO CONTRÁRIO...
  "WRITE 'AS DEDUÇÕES SALARIAIS NÃO FORAM ENCONTRADAS.'. "ESCREVA A MENSAGEM."
ENDIF.                                                  "ENCERRA ESTRUTURA DE CONTROLE."

*"------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
"[LER A PRIMEIRA ENTRADA]"
READ TABLE LT_TABLE INTO LS_STRUCT INDEX 1. "LER O PRIMEIRO ELEMENTO DA TABELA TRATANDO-A COMO UM ARRAY."

IF SY-SUBRC = 0.                                        "SE A INSTRUÇÃO FOR BEM SUCEDIDA...
  "WRITE LS_STRUCT-DEDUCTIONS.                           "EXIBE A MENSAGEM."
ELSE.                                                   "DO CONTRÁRIO...
  "WRITE 'AS DEDUÇÕES SALARIAIS NÃO FORAM ENCONTRADAS.'. "ESCREVA A MENSAGEM."
ENDIF.                                                  "ENCERRA ESTRUTURA DE CONTROLE."

*"------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
"[LER A ENTRADA POR ÍNDICE NUMÉRICO]"
READ TABLE LT_TABLE INTO LS_STRUCT INDEX LV_INDEX. "LER O PRIMEIRO ELEMENTO DA TABELA COMO UM ARRAY."

IF SY-SUBRC = 0.                                        "SE A INSTRUÇÃO FOR BEM SUCEDIDA...
  "WRITE LS_STRUCT-DEDUCTIONS.                           "EXIBE A MENSAGEM.
ELSE.                                                   "DO CONTRÁRIO...
  "WRITE 'AS DEDUÇÕES SALARIAIS NÃO FORAM ENCONTRADAS.'. "ESCREVA A MENSAGEM."
ENDIF.                                                  "ENCERRA ESTRUTURA DE CONTROLE."

*"------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
"[LER A ENTRADA POR PESQUISA BINÁRIA]"
READ TABLE LT_TABLE INTO LS_STRUCT WITH KEY ID = '2' BINARY SEARCH.
"LER A LINHA DA TABELA INTERNA ONDE O ID É 2 USANDO PESQUISA BINÁRIA."

IF SY-SUBRC = 0.                                          "SE A INSTRUÇÃO FOR BEM SUCEDIDA...
  "WRITE LS_STRUCT-DEDUCTIONS.                             "EXIBE A MENSAGEM.
ELSE.                                                     "DO CONTRÁRIO...
  "WRITE / 'AS DEDUÇÕES SALARIAIS NÃO FORAM ENCONTRADAS.'. "ESCREVA A MENSAGEM."
ENDIF.                                                    "ENCERRA ESTRUTURA DE CONTROLE."

SKIP 1.

*"------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------

"[EXERCÍCIO: GERENCIAMENTO DE FOLHA DE PAGAMENTO]"

"VOCÊ FOI ENCARREGADO DE DESENVOLVER UM PROGRAMA ABAP PARA GERENCIAR OS DADOS DE FOLHA DE PAGAMENTO
"DE UMA EMPRESA UTILIZANDO INFORMAÇÕES DA TABELA PAYROLL.
"ESTA TABELA CONTÉM INFORMAÇÕES BÁSICAS SOBRE OS FUNCIONáRIOS, INCLUINDO DADOS
"RELEVANTES PARA A FOLHA DE PAGAMENTO.

"SEU PROGRAMA DEVE SER CAPAZ DE EXECUTAR AS SEGUINTES TAREFAS:

"1 - EXIBIR AS DEDUÇÕES SALARIAIS DO FUNCIONÁRIO COM O MAIOR NUMERO DE ID MENOR DO QUE 100."

"2 - EXIBIR AS DEDUÇÕES SALARIAIS DO FUNCIONáRIO COM ID íMPAR MAIS PRóXIMO DE 100.

"3 - EXIBIR AS DEDUÇÕES SALARIAIS DO FUNCIONáRIO COM ID PAR MAIS PRóXIMO DO PRIMEIRO FUNCIONáRIO NA TABELA.

"4 - EXIBIR AS DEDUÇÕES SALARIAIS DO FUNCIONáRIO COM O ID íMPAR MAIS PRóXIMO DO PRIMEIRO FUNCIONáRIO.

"5 - REALIZAR UMA PESQUISA BINÁRIA PARA ENCONTRAR AS DEDUÇÕES SALARIAIS DOS FUNCIONáRIOS COM O NÚMERO DE ID
"PAR MAIS PRÓXIMO DE 10 E EXIBI-LAS.

"6 - REALIZAR UMA PESQUISA BINÁRIA PARA ENCONTRAR AS DEDUÇÕES SALARIAIS DOS FUNCIONáRIOS COM O NÚMERO DE ID
"íMPAR MAIS PRÓXIMO DE 9 E EXIBI-LAS.

"CERTIFIQUE-SE DE LIDAR ADEQUADAMENTE COM CASOS EM QUE OS DADOS NãO SãO ENCONTRADOS E CONSIDERE A EFICIêNCIA
"DO ALGORITMO AO REALIZAR AS OPERAÇÕES DE PESQUISA BINÁRIA.

*"------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------

"LER E EXIBIR AS DEDUÇÕES SALARIAIS DO FUNCIONÁRIO COM O MAIOR NUMERO DE ID MENOR DO QUE 100."

DATA: LT_TABLE2 TYPE TABLE OF ZPAYROLL, "TABELA INTERNA DO TIPO DA TABELA TRANSPARENTE DE PAYROLL."
      LS_STRUCT2 TYPE ZPAYROLL,         "ESTRUTURA LOCAL DO TIPO DA TABELA TRANSPARENTE DE PAYROLL."
      MAXID TYPE ZID.                   "VARIÁVEL PARA PEGAR O VALOR MÁXIMO DO ID."

SELECT * FROM ZPAYROLL INTO TABLE LT_TABLE2. "PREENCHENDO TABELA INTERNA."
SELECT MAX( ID ) FROM ZPAYROLL INTO MAXID.   "PREENCHENDO TABELA INTERNA."

READ TABLE LT_TABLE2 INTO LS_STRUCT2 WITH KEY ID = MAXID. "PROCURA NA TABELA INTERNA O VALOR DO ID."

WRITE / '1.........................................................'.

IF SY-SUBRC = 0.   "SE A INSTRUÇÃO FOR BEM SUCEDIDA...
  IF MAXID >= 100. "E SE A VARIÁVEL FOR MENOR OU IGUAL A 100...
    WRITE / 'NÃO FOI POSSÍVEL ENCONTRAR OS DADOS SOLICITADOS.'. "EXIBE A MENSAGEM."
  ELSE. "DO CONTRÁRIO...
    WRITE: / 'DEDUÇÂO SALARIAL DO FUNCIONáRIO', LS_STRUCT2-ID,'->', LS_STRUCT2-DEDUCTIONS,'€'.
  ENDIF. "ENCERRA ESTRUTURA DE CONTROLE."
ELSE. "SE NÃO ACHAR ID NA TABELA INTERNA."
  WRITE / 'NÃO EXISTE ID NA TABELA.'. "EXIBE A MENSAGEM."
ENDIF. "ENCERRA ESTRUTURA DE CONTROLE."
WRITE / '..........................................................'.

*"------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------

"LER E EXIBIR AS DEDUÇÕES SALARIAIS DO FUNCIONáRIO COM O MAIOR ID íMPAR MENOR DO QUE 100.

DATA: INDEX TYPE ZID,               "VARIÁVEL NUMÉRICA PARA RECEBER O COMEÇO DE UM ÍNDICE."
      ENDINDEX TYPE ZID,            "VARIÁVEL NUMÉRICA PARA RECEBER O FINAL DE UM ÍNDICE."
      DEDUCTIONS1 TYPE ZDEDUCTIONS. "VARIÁVEL PARA RECEBER O VALOR DAS DEDUÇÕES SALARIAIS."

TYPES: BEGIN OF GS_STRUCT3,            "CRIAÇÃO DE ESTRUTURA TYPES."
          ID TYPE ZID,                 "ID RECEBE O TIPO DO ID DA TABELA TRANSPARENTE."
          DEDUCTIONS TYPE ZDEDUCTIONS, "DEDUCOES RECEBE O TIPO DE DEDUCOES DA TABELA TRANSPARENTE."
       END OF GS_STRUCT3.              "ENCERRAMENTO DA ESTRUTURA."

DATA: LT_TABLE3 TYPE TABLE OF GS_STRUCT3, "TABELA INTERNA DO TIPO DA ESTRUTURA TYPES."
      LS_STRUCT3 LIKE LINE OF LT_TABLE3.  "ESTRUTURA LOCAL DO TIPO LINHA DA MINHA TABELA INTERNA.
                                          "ESTRUTURA LOCAL TAMBÉM PODERIA SER DO TIPO DA ESTRUTURA TYPES."

SELECT ID DEDUCTIONS FROM ZPAYROLL INTO TABLE LT_TABLE3.                "PREENCHENDO TABELA INTERNA."
                                                                        "OU...
SELECT ID, DEDUCTIONS FROM ZPAYROLL INTO TABLE @DATA(LT_TABLE4_INLINE). "PREENCHENDO TABELA INTERNA."

SELECT MIN( ID ) FROM ZPAYROLL INTO INDEX.    "SELECIONA O MENOR NUMERO DA COLUNA ID"
SELECT MAX( ID ) FROM ZPAYROLL INTO ENDINDEX. "SELECIONA O MAIOR NÚMERO DA COLUNA ID"

SORT LT_TABLE3 BY ID. "ORDENA A TABELA INTERNA POR ID."

WHILE INDEX < ENDINDEX.                             "ENQUANTO NÃO CHEGAR AO FIM DA TABELA...
  READ TABLE LT_TABLE3 INTO LS_STRUCT3 INDEX INDEX. "ITERA NA TABELA INTERNA RECEBENDO O VALOR DO INDEX."
  INDEX = INDEX + 1.                                "INCREMENTA O INDEX."
  IF INDEX MOD 2 NE 0.                              "SE O INDEX FOR ÍMPAR."
    DEDUCTIONS1 = LS_STRUCT3-DEDUCTIONS.            "VARIÁVEL RECEBE O VALOR DAS DEDUÇÕES SALARIAIS."
  ENDIF.                                            "ENCERRA ESTRUTURA DE CONTROLE."
ENDWHILE.                                           "ENCERRA O CICLO."

WRITE / '2.........................................................'.
WRITE: / 'DEDUÇÂO SALARIAL DO FUNCIONáRIO', LS_STRUCT3-ID,'->', DEDUCTIONS1,'€'. "EXIBE A MENSAGEM."
WRITE / '..........................................................'.

*"------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------

"LER E EXIBIR AS DEDUÇÕES SALARIAIS DO FUNCIONáRIO COM O NúMERO ID
"PAR MAIS PRóXIMO DO PRIMEIRO FUNCIONÁRIO NA TABELA.

SELECT ID, DEDUCTIONS FROM ZPAYROLL INTO TABLE @DATA(LT_TABLE4). "PREENCHENDO TABELA INTERNA."

DATA: LS_STRUCT4 LIKE LINE OF LT_TABLE4, "ESTRUTURA DO TIPO DA TABELA INTERNA DINÂMICA."
      BEGIN TYPE ZID,                    "VARIÁVEL NUMÉRICA O MENOR NÚMERO DO ID."
      END TYPE ZID,                      "VARIÁVEL NUMÉRICA PARA RECEBER O O MAIOR NÚMERO DO ID.."
      DEDUCTIONS2 TYPE ZDEDUCTIONS,      "VARIÁVEL PARA RECEBER O VALOR DAS DEDUÇÕES SALARIAIS."
      ID4 TYPE ZID.                      "VARIÁVEL PARA RECEBER O VALOR DO ÚLTIMO ID PAR."

SORT LT_TABLE4 BY ID DESCENDING. "ORDENA A TABELA POR ID DESCRESCENTE."

SELECT MIN( ID ) FROM ZPAYROLL INTO BEGIN. "SELECIONA O MENOR NUMERO DA COLUNA ID.
SELECT MAX( ID ) FROM ZPAYROLL INTO END.   "SELECIONA O MAIOR NÚMERO DA COLUNA ID.

WHILE END > BEGIN.                                        "ENQUANTO NÃO CHEGAR AO FIM DA TABELA...
  READ TABLE LT_TABLE4 INTO LS_STRUCT4 WITH KEY ID = END. "ITERA NA TABELA INTERNA RECEBENDO O VALOR DO ID."
  IF END MOD 2 = 0.                                       "SE O ID FOR PAR."
    DEDUCTIONS2 = LS_STRUCT4-DEDUCTIONS.                  "VARIÁVEL RECEBE O VALOR DAS DEDUÇÕES SALARIAIS."
    ID4 = LS_STRUCT4-ID.                                  "VARIÁVEL RECEBE O VALOR DO ID."
  ENDIF.                                                  "ENCERRA ESTRUTURA DE CONTROLE."
  END = END - 1.                                          "DECREMENTA O ITERADOR."
ENDWHILE.                                                 "ENCERRA O CICLO."

LS_STRUCT4-ID = LS_STRUCT4-ID + 1. "CORREÇÃO PARA EVITAR UM NÚMERO NEGATIVO."

WRITE / '3.........................................................'.
WRITE: / 'DEDUÇÂO SALARIAL DO FUNCIONáRIO', ID4,'->', DEDUCTIONS2,'€'. "EXIBE A MENSAGEM."
WRITE / '..........................................................'.

*"------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------

"LER E EXIBIR AS DEDUÇÕES SALARIAIS DO FUNCIONáRIO COM O NúMERO DE ID
"íMPAR MAIS PRóXIMO DO PRIMEIRO FUNCIONáRIO NA TABELA.

SELECT ID, DEDUCTIONS FROM ZPAYROLL INTO TABLE @DATA(LT_TABLE5). "PREENCHENDO TABELA INTERNA."

DATA: LS_STRUCT5 TYPE GS_STRUCT3,   "ESTRUTURA DO TIPO DA TABELA INTERNA DINÂMICA."
      BEGIN5 TYPE ZID,              "VARIÁVEL NUMÉRICA O MENOR NÚMERO DO ID."
      END5 TYPE ZID,                "VARIÁVEL NUMÉRICA PARA RECEBER O O MAIOR NÚMERO DO ID.."
      DEDUCTIONS5 TYPE ZDEDUCTIONS, "VARIÁVEL PARA RECEBER O VALOR DAS DEDUÇÕES SALARIAIS."
      ID5 TYPE ZID.                 "VARIÁVEL PARA RECEBER O VALOR DO ÚLTIMO ID PAR."

SORT LT_TABLE5 BY ID ASCENDING. "ORDENA A TABELA POR ID ASCENDENTE."

SELECT MIN( ID ) FROM ZPAYROLL INTO BEGIN5. "SELECIONA O MENOR NUMERO DA COLUNA ID.
SELECT MAX( ID ) FROM ZPAYROLL INTO END5.   "SELECIONA O MAIOR NÚMERO DA COLUNA ID.

WHILE END5 > BEGIN5.                                       "ENQUANTO NÃO CHEGAR AO FIM DA TABELA...
  READ TABLE LT_TABLE5 INTO LS_STRUCT5 WITH KEY ID = END5. "ITERA NA TABELA INTERNA RECEBENDO O VALOR DO ID."
  IF END5 MOD 2 NE 0 AND END5 NE 0.                        "SE O ID FOR ÍMPAR E DIFERENTE DE ZERO...
    DEDUCTIONS5 = LS_STRUCT5-DEDUCTIONS.                   "VARIÁVEL RECEBE O VALOR DAS DEDUÇÕES SALARIAIS."
    ID5 = LS_STRUCT5-ID.                                   "VARIÁVEL RECEBE O VALOR DO ID."
  ENDIF.                                                   "ENCERRA ESTRUTURA DE CONTROLE."
  END5 = END5 - 1.                                         "DECREMENTA O ITERADOR."
ENDWHILE.                                                  "ENCERRA O CICLO."

WRITE / '4.........................................................'.
WRITE: / 'DEDUÇÂO SALARIAL DO FUNCIONáRIO', ID5,'->', DEDUCTIONS5,'€'. "EXIBE A MENSAGEM."
WRITE / '..........................................................'.

*"------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------

"REALIZAR UMA PESQUISA BINÁRIA PARA ENCONTRAR AS DEDUÇÕES SALARIAIS DOS FUNCIONáRIOS COM O NÚMERO DE ID
"PAR MAIS PRÓXIMO DE 10 E EXIBI-LAS.

SELECT ID, DEDUCTIONS FROM ZPAYROLL INTO TABLE @DATA(LT_TABLE6). "PREENCHENDO TABELA INTERNA."

DATA: END6 TYPE ZID,                "VARIÁVEL NUMÉRICA PARA RECEBER O O MAIOR NÚMERO DO ID.."
      DEDUCTIONS6 TYPE ZDEDUCTIONS, "VARIÁVEL PARA RECEBER O VALOR DAS DEDUÇÕES SALARIAIS."
      ID6 TYPE ZID.                 "VARIÁVEL PARA RECEBER O VALOR DO ÚLTIMO ID PAR."

SORT LT_TABLE6 BY ID ASCENDING. "ORDENA A TABELA POR ID ASCENDENTE."

SELECT MAX( ID ) FROM ZPAYROLL INTO END6. "SELECIONA O MAIOR NÚMERO DA COLUNA ID.

WHILE END6 > 11.
  READ TABLE LT_TABLE6 INTO DATA(LS_STRUCT6) WITH KEY ID = END6 BINARY SEARCH.
  IF END6 MOD 2 EQ 0.
    DEDUCTIONS6 = LS_STRUCT6-DEDUCTIONS.
    ID6 = LS_STRUCT6-ID.
  ENDIF.
  END6 = END6 - 1.
ENDWHILE.

"ENQUANTO NÃO CHEGAR AO AO ID 11 DA TABELA...
"ITERA NA TABELA INTERNA RECEBENDO O VALOR DO ID."
"SE O ID FOR PAR...
"VARIÁVEL RECEBE O VALOR DAS DEDUÇÕES SALARIAIS."
"VARIÁVEL RECEBE O VALOR DO ID."
"ENCERRA ESTRUTURA DE CONTROLE."
"DECREMENTA O ITERADOR."
"ENCERRA O CICLO."

WRITE / '5.........................................................'.
WRITE: / 'DEDUÇÂO SALARIAL DO FUNCIONáRIO', ID6,'->', DEDUCTIONS6,'€'. "EXIBE A MENSAGEM."

END6 = 0. "REINICIA A VARIÁVEL RECEBENDO O ZERO."

WHILE END6 < 10.
  READ TABLE LT_TABLE6 INTO DATA(LS_STRUCT6_2) WITH KEY ID = END6 BINARY SEARCH.
  IF END6 MOD 2 EQ 0.
    DEDUCTIONS6 = LS_STRUCT6_2-DEDUCTIONS.
    ID6 = LS_STRUCT6_2-ID.
  ENDIF.
  END6 = END6 + 1.
ENDWHILE.

"ENQUANTO NÃO CHEGAR AO ID 10 DA TABELA...
"ITERA NA TABELA INTERNA CRIANDO UMA NOVA ESTRUTURA DINÂMICA RECEBENDO O VALOR DO ID."
"SE O ID FOR PAR.
"VARIÁVEL RECEBE O VALOR DAS DEDUÇÕES SALARIAIS."
"VARIÁVEL RECEBE O VALOR DO ID."
"ENCERRA ESTRUTURA DE CONTROLE."
"INCREMENTA O ITERADOR."
"ENCERRA O CICLO."

WRITE: / 'DEDUÇÂO SALARIAL DO FUNCIONáRIO', ID6,'->', DEDUCTIONS6,'€'. "EXIBE A MENSAGEM."
WRITE / '..........................................................'.

*"------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------

"REALIZAR UMA PESQUISA BINÁRIA PARA ENCONTRAR AS DEDUÇÕES SALARIAIS DOS FUNCIONáRIOS COM O NÚMERO DE
"ID íMPAR MAIS PRÓXIMO DE 9 E EXIBI-LAS.

SELECT ID, DEDUCTIONS FROM ZPAYROLL INTO TABLE @DATA(LT_TABLE7). "PREENCHENDO TABELA INTERNA."

DATA: END7 TYPE ZID,                "VARIÁVEL NUMÉRICA PARA RECEBER O O MAIOR NÚMERO DO ID.."
      DEDUCTIONS7 TYPE ZDEDUCTIONS, "VARIÁVEL PARA RECEBER O VALOR DAS DEDUÇÕES SALARIAIS."
      ID7 TYPE ZID.                 "VARIÁVEL PARA RECEBER O VALOR DO ÚLTIMO ID PAR."

SORT LT_TABLE7 BY ID ASCENDING. "ORDENA A TABELA POR ID ASCENDENTE."

SELECT MAX( ID ) FROM ZPAYROLL INTO END7.   "SELECIONA O MAIOR NÚMERO DA COLUNA ID,

WHILE END7 > 9.
  READ TABLE LT_TABLE7 INTO DATA(LS_STRUCT7) WITH KEY ID = END7 BINARY SEARCH.
  IF END7 MOD 2 NE 0.
    DEDUCTIONS7 = LS_STRUCT7-DEDUCTIONS.
    ID7 = LS_STRUCT7-ID.
  ENDIF.
  END7 = END7 - 1.
ENDWHILE.

"ENQUANTO NÃO CHEGAR AO AO ID 11 DA TABELA...
"ITERA NA TABELA INTERNA RECEBENDO O VALOR DO ID."
"SE O ID FOR ÍMPAR...
"VARIÁVEL RECEBE O VALOR DAS DEDUÇÕES SALARIAIS."
"VARIÁVEL RECEBE O VALOR DO ID."
"ENCERRA ESTRUTURA DE CONTROLE."
"DECREMENTA O ITERADOR."
"ENCERRA O CICLO."

WRITE / '6.........................................................'.
WRITE: / 'DEDUÇÂO SALARIAL DO FUNCIONáRIO', ID7,'->', DEDUCTIONS7,'€'. "EXIBE A MENSAGEM."

END7 = 0. "REINICIA A VARIÁVEL RECEBENDO O ZERO."

WHILE END7 < 9.
  READ TABLE LT_TABLE7 INTO DATA(LS_STRUCT7_2) WITH KEY ID = END7 BINARY SEARCH.
  IF END7 MOD 2 NE 0.
    DEDUCTIONS7 = LS_STRUCT7_2-DEDUCTIONS.
    ID7 = LS_STRUCT7_2-ID.
  ENDIF.
  END7 = END7 + 1.
ENDWHILE.

"ENQUANTO NÃO CHEGAR AO ID 10 DA TABELA...
"ITERA NA TABELA INTERNA CRIANDO UMA NOVA ESTRUTURA DINÂMICA RECEBENDO O VALOR DO ID."
"SE O ID FOR ÍMPAR.
"VARIÁVEL RECEBE O VALOR DAS DEDUÇÕES SALARIAIS."
"VARIÁVEL RECEBE O VALOR DO ID."
"ENCERRA ESTRUTURA DE CONTROLE."
"INCREMENTA O ITERADOR."
"ENCERRA O CICLO."

WRITE: / 'DEDUÇÂO SALARIAL DO FUNCIONáRIO', ID7,'->', DEDUCTIONS7,'€'. "EXIBE A MENSAGEM."
WRITE / '..........................................................'.

*"------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
