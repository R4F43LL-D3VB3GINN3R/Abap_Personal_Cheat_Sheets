*&---------------------------------------------------------------------*
*& Report ZREPORT_5
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zreport_5.

*5 - Elaborar um programa ABAP onde deverá ser criada uma tela de seleção com
*o Nº conta do Razão como seleção múltipla. Seu tipo se encontra na tabela SKA1.

TABLES: ska1, "Tabela Transparente - Mestre de Contas do Razão
        skb1, "Tabela Transparente - Mestre da Conta do Razão (Empresa)
        t001, "Tabela Transparente - Empresas
        skat. "Tabela Transparente - Mestre de Contas do Razão (Denominação)

"---------------
"Tela de Seleção
"---------------

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS s_saknr FOR ska1-saknr DEFAULT '0000001000'. "Campo de Escolha Múltipla - Nº Conta do Razão
SELECTION-SCREEN END OF BLOCK a1.
SELECTION-SCREEN SKIP 1.

*Selecionar na tabela SKA1 todas as Contas do Razão que estiverem de acordo
*com o campo Nº conta do Razão da tela de seleção e que pertençam ao Plano de
*Contas ‘INT’, retornando os campos Nº conta do Razão e Data de criação do registro.

TYPES: BEGIN OF ty_ska1, "Estrutura - Mestre de Contas do Razão
  saknr TYPE ska1-saknr, "Nº conta do Razão
  erdat TYPE ska1-erdat, "Data de Criação do Registro
END OF ty_ska1.

DATA: t_ska1  TYPE TABLE OF ty_ska1, "Tabela Interna - Estrutura - Mestre de Contas do Razão
      ls_ska1 TYPE ty_ska1.          "Estrutura - Estrutura - Mestre de Contas do Razão

SELECT saknr,      "Selecione Nº conta do Razão
       erdat       "Data de Criação do Registro
       FROM ska1   "Da Tabela Transparente - Mestre de Contas do Razão
       INTO CORRESPONDING FIELDS OF TABLE @t_ska1 "Nos campos correspondentes da Tabela Interna - Mestre de Contas do Razão
       WHERE saknr IN @s_saknr "Onde o Nº conta do Razão está no range do Campo de Escolha Múltipla - Nº Conta do Razão
       AND ktopl = 'INT'. "E Plano de Contas for 'INT'

*Para cada registro encontrado na tabela SKA1, selecionar na tabela SKB1 as
*empresas correspondentes as Contas do Razão encontradas onde o campo Nº conta do
*Razão relaciona as duas tabelas, retornando os campos Empresa e Nº conta do Razão.

TYPES: BEGIN OF ty_skb1, "Estrutura - Mestre da Conta do Razão (Empresa)
  bukrs TYPE skb1-bukrs, "Empresa
  saknr TYPE skb1-saknr, "Nº conta do Razão
END OF ty_skb1.

DATA: t_skb1  TYPE TABLE OF ty_skb1, "Tabela Interna - Estrutura - Mestre da Conta do Razão (Empresa)
      ls_skb1 TYPE ty_skb1.          "Estrutura - Estrutura - Mestre da Conta do Razão (Empresa)

SELECT bukrs,    "Seleciona Empresa
       saknr     "Nº conta do Razão
       FROM skb1 "Da Tabela Transparente - Mestre de Contas do Razão (Empresa)
       INTO CORRESPONDING FIELDS OF TABLE @t_skb1 "Nos campos correspondentes da Tabela Interna - Mestre de Contas do Razão (Empresa)
       FOR ALL ENTRIES IN @t_ska1 "Relacionado com a Tabela Interna - Mestre de Contas do Razão
       WHERE saknr = @t_ska1-saknr. "Onde os campos Nº conta do Razão são iguais.

*Para cada registro encontrado na tabela SKB1, selecionar na tabela T001 os
*dados das empresas selecionadas, onde o campo Empresa relaciona as duas tabelas e
*o País seja igual a ‘BR’, retornando os campos Empresa e Denominação da firma ou
*empresa.

TYPES: BEGIN OF ty_t001, "Estrutura - Empresas
  bukrs TYPE t001-bukrs, "Empresa
  butxt TYPE t001-butxt, "Denominação da Firma ou Empresa
END OF ty_t001.

DATA: t_t001  TYPE TABLE OF ty_t001, "Tabela Interna - Estrutura - Empresa
      ls_t001 TYPE ty_t001.          "Estrutura - Estrutura - Empresa

SELECT bukrs,    "Selecione Empresa
       butxt     "Denominação da Firma ou Empresa
       FROM t001 "Da Tabela Transparente - Empresa
       INTO CORRESPONDING FIELDS OF TABLE @t_t001 "Nos campos correspondentes da Tabela Interna - Empresa
       FOR ALL ENTRIES IN @t_skb1 "Relacionado com a Tabela Interna - Mestre de Contas do Razão (Empresa)
       WHERE bukrs = @t_skb1-bukrs "Onde as Empresas são as mesmas
       AND land1 = 'BR'. "E onde o país é Brasil

*Para cada registro retornado da tabela SKA1, selecionar na tabela SKAT sua
*descrição desde que esta esteja no Código de idioma ‘PT’, Plano de Contas ‘INT’, onde
*o campo Nº conta do Razão relaciona as duas tabelas, retornando os campos Nº conta
*do Razão e Texto das contas do Razão.

TYPES: BEGIN OF ty_skat, "Estrutura - Mestre de Contas do Razão (Denominação)
  saknr TYPE skat-saknr, "Nº conta do Razão
  txt20 TYPE skat-txt20, "Texto das Contas do Razão
END OF ty_skat.

DATA: t_skat  TYPE TABLE OF ty_skat, "Tabela Interna - Estrutura - Mestre de Contas do Razão (Denominação)
      ls_skat TYPE ty_skat.          "Estrutura - Estrutura - Mestre de Contas do Razão (Denominação)

SELECT saknr,    "Seleciona o Nº conta do Razão
       txt20     "Texto das Contas do Razão
       FROM skat "Da Tabela Transparente - Mestre de Contas do Razão (Denominação)
       INTO CORRESPONDING FIELDS OF TABLE @t_skat "Nos campos correspondentes da Tabela Interna - Mestre de Contas do Razão (Denominação)
       FOR ALL ENTRIES IN @t_ska1 "Relacionado com a Tabela Interna - Mestre de Contas do Razão
       WHERE saknr = @t_ska1-saknr "Onde os Nº conta do Razão são iguais
       AND ktopl = 'INT' "E Plano de Contas = INT
       AND spras = 'P'.  "Este filtro foi removido"

TYPES: BEGIN OF ty_output, "Estrutura - Tabela de Saída
  saknr TYPE ska1-saknr,   "Nº conta do Razão
  erdat TYPE ska1-erdat,   "Data de Criação do Registro
  bukrs TYPE skb1-bukrs,   "Empresa
  butxt TYPE t001-butxt,   "Denominação da Firma ou Empresa
  txt20 TYPE skat-txt20,   "Texto das Contas do Razão
END OF ty_output.

DATA: t_output  TYPE TABLE OF ty_output, "Tabela Interna - Estrutura - Tabela de Saída
      ls_output TYPE ty_output.          "Estrutura - Estrutura - Tabela de Saída

"Enche a Tabela de Saída
LOOP AT t_ska1 INTO ls_ska1.
  CLEAR ls_output.
  LOOP AT t_skb1 INTO ls_skb1 WHERE saknr = ls_ska1-saknr.
    ls_output-saknr = ls_ska1-saknr.
    ls_output-erdat = ls_ska1-erdat.
    ls_output-bukrs = ls_skb1-bukrs.
    LOOP AT t_t001 INTO ls_t001 WHERE bukrs = ls_skb1-bukrs.
      ls_output-butxt = ls_t001-butxt.
    ENDLOOP.
  ENDLOOP.
  LOOP AT t_skat INTO ls_skat WHERE saknr = ls_ska1-saknr.
    ls_output-txt20 = ls_skat-txt20.
    IF sy-subrc = 0.
      APPEND ls_output TO t_output.
    ENDIF.
  ENDLOOP.
ENDLOOP.

*O relatório deve imprimir todas as contas do razão de cada empresa selecionada
*do País ‘BR’, bem como os dados da empresa e a descrição da conta do razão no
*Código de idioma ‘PT’. Para cada empresa deverá mostrar um contador de contas do
*razão e no final do relatório a quantidade de registros encontrados.
*Imprimir os campos: Empresa, Denominação da firma ou empresa, Nº conta do
*Razão, Data de criação do registro e Texto das contas do Razão.
*Exemplo de Layout:

DATA: lv_count TYPE i,
      lv_last_bukrs TYPE skb1-bukrs.

SORT t_output BY bukrs.

LOOP AT t_output INTO ls_output.
  WRITE: / 'Empresa:', ls_output-bukrs,
         / 'Denominação da Firma ou Empresa:', ls_output-butxt,
         / 'Nº conta do Razão:', ls_output-saknr,
         / 'Data de Criação do Registro:', ls_output-erdat,
         / 'Texto das Contas do Razão:', ls_output-txt20.
  WRITE: / '-----------------------------------------------'.
ENDLOOP.
