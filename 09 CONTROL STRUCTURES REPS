*&-----------------------------------------------------------------------------------------------------------*
*"------------------------------------------------------------------------------------------------------------
*"--------------------------------ESTRUTURAS-DE-CONTROLE-DE-REPETIÇÃO-TUTORIAL--------------------------------
*"------------------------------------------------------------------------------------------------------------
*"RAFAEL-ALBUQUERQUE------------------------------------------------------------------------------------------
*"06-03-2024--------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
REPORT ZTEST3.

"-------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------

" TRANSPARENT TABLE - PA0002. [HCM MODULE]

TYPES: BEGIN OF TY_PA0002,       "ESTRUTURA TYPES.
         PERNR   TYPE PERNR_D,   "NUMERO PESSOAL [ID]
         VORNA   TYPE PAD_VORNA, "NOME
         GBDAT   TYPE GBDAT,     "DATA DE NASCIMENTO
         GESCH   TYPE GESCH,     "SEXO
         NATIO   TYPE LAND1,     "NACIONALIDADE
         FAMST   TYPE FAMST,     "ESTADO CIVIL
         BEGDA   TYPE BEGDA,     "DATA DE INÍCIO DE VALIDADE
         ENDDA   TYPE ENDDA,     "DATA DE TÉRMINO DE VALIDADE
       END OF ty_PA0002.

DATA: LT_DATA TYPE TABLE OF TY_PA0002, "TABELA INTERNA."
      LS_DATA LIKE LINE OF LT_DATA.    "ESTRUTURA LOCAL."

"SELECIONA OS CAMPOS DA TABELA TRANSPARENTE E INSERE NA TABELA INTERNA."
SELECT PA0002~PERNR
       PA0002~VORNA
       PA0002~GBDAT
       PA0002~GESCH
       PA0002~NATIO
       PA0002~FAMST
       PA0002~BEGDA
       PA0002~ENDDA FROM PA0002 INTO TABLE LT_DATA.

"-------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
"[REPETIR POR QUANTIDADE DE VEZES DETERMINADA]"

DATA: CONTADOR TYPE I. "VARIÁVEL INTEIRO PARA RECEBER CONTAGEM."

CONTADOR = 0. "VARIÁVEL RECEBE VALOR INICIAL DE CONTAGEM."

DO 10 TIMES.               "FAZER 10 VEZES...
  WRITE / CONTADOR.        "ESCREVE O VALOR DO CONTADOR."
  CONTADOR = CONTADOR + 1. "INCREMENTA O CONTADOR."
ENDDO.                     "ENCERRA ESTRUTURA DE CONTROLE."

"-------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
"[REPETIR ENQUANTO CONDIÇÃO É ESTABELECIDA]"

DATA: CONTADOR2 TYPE I. "VARIÁVEL INTEIRO PARA RECEBER CONTAGEM."

CONTADOR2 = 0. "VARIÁVEL RECEBE VALOR INICIAL DE CONTAGEM."

WHILE CONTADOR2 <= 10.       "ENQUANTO O CONTADOR FOR MENOR OU IGUAL A 10."
  WRITE / CONTADOR2.         "ESCREVE O VALOR DO CONTADOR."
  CONTADOR2 = CONTADOR2 + 1. "INCREMENTA O CONTADOR."
ENDWHILE.                    "ENCERRA ESTRUTURA DE CONTROLE."

"-------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
"[ITERA SOBRE TABELA INTERNA OU FAIXA DE NÚMEROS.]"

DATA CONTADOR3 TYPE I. "VARIÁVEL INTEIRO PARA RECEBER CONTAGEM."

  CONTADOR3 = 1. "VARIÁVEL RECEBE VALOR INICIAL DE CONTAGEM."

LOOP AT LT_DATA INTO LS_DATA. "ITERAR SOBRE A TABELA LT_DATA."
    WRITE: / '----------------------------------------------------', "EXIBE OS DADOS DA TABELA."
           / 'FUNCIONÁRIO: ', CONTADOR3,
           / '----------------------------------------------------',
           / 'Número Pessoal (ID do funcionário):', LS_DATA-PERNR,
           / 'Nome do funcionário:', LS_DATA-VORNA,
           / 'Data de nascimento:', LS_DATA-GBDAT,
           / 'Sexo:', LS_DATA-GESCH,
           / 'Nacionalidade:', LS_DATA-NATIO,
           / 'Estado civil:', LS_DATA-FAMST,
           / 'Data de início de validade:', LS_DATA-BEGDA,
           / 'Data de término de validade:', LS_DATA-ENDDA,
           / '-----------------------------------------------------'.
    CONTADOR3 = CONTADOR3 + 1. "INCREMENTA O CONTADOR."
  ENDLOOP. "ENCERRA ESTRUTURA DE CONTROLE."

"-------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
"[ITERA SOBRE TABELA INTERNA COM ESTRUTURA DINÂMICA.]"

LOOP AT LT_DATA INTO DATA(DYN_DATA). "ITERAR SOBRE A TABELA LT_DATA."
                                     "A ESTRUTURA DYN_DATA FOI CRIADA NO MOMENTO DO ENUNCIADO."
                                     "DYN_DATA RECEBE AS CARACTERÍSTICAS DE LINHA DA TABELA INTERNA."

    WRITE: / '----------------------------------------------------', "EXIBE OS DADOS DA TABELA."
           / 'FUNCIONÁRIO: ', CONTADOR3,
           / '----------------------------------------------------',
           / 'Número Pessoal (ID do funcionário):', DYN_DATA-PERNR,
           / 'Nome do funcionário:', DYN_DATA-VORNA,
           / 'Data de nascimento:', DYN_DATA-GBDAT,
           / 'Sexo:', DYN_DATA-GESCH,
           / 'Nacionalidade:', DYN_DATA-NATIO,
           / 'Estado civil:', DYN_DATA-FAMST,
           / 'Data de início de validade:', DYN_DATA-BEGDA,
           / 'Data de término de validade:', DYN_DATA-ENDDA,
           / '-----------------------------------------------------'.
    CONTADOR3 = CONTADOR3 + 1. "INCREMENTA O CONTADOR."

    "PS: O CONTADOR AQUI SERVE APENAS PARA CONTAR A QUANTIDADE DE REGISTROS APRESENTADOS.
        "NÃO SE FAZ NECESSÁRIO USÁ-LO PORQUE O LOOP ITERA SOBRE A TABELA INDEPENDENTEMENTE."

  ENDLOOP. "ENCERRA ESTRUTURA DE CONTROLE."

"-------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
"[ITERA SOBRE TABELA INTERNA USANDO UMA ESTRUTURA DINÂMICA E UM UM FILTRO DE PESQUISA.]"

LOOP AT LT_DATA INTO DATA(DYN_DATA2) WHERE VORNA = 'Doe'. "ITERAR SOBRE A TABELA LT_DATA COM FILTRO."
  WRITE: / 'ESTADO CIVIL: ', DYN_DATA2-FAMST. "EXIBE O ESTADO CIVIL"
  ENDLOOP. "ENCERRA ESTRUTURA DE CONTROLE."

"-------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
"[ESTRUTURA DE CONTROLE DE REPETIÇÃO COM CONTROLE DE SAÍDA.]"

DATA CONTADOR4 TYPE I. "VARIÁVEL INTEIRO PARA RECEBER CONTAGEM."

CONTADOR4 = 1. "VARIÁVEL RECEBE VALOR INICIAL DE CONTAGEM."

DO 10 TIMES.                   "FAZER 10 VEZES...
  WRITE / CONTADOR4.           "EXIBE OS NÚMEROS."
  IF CONTADOR4 EQ 5.           "SE O CONTADOR FOR IGUAL A 5...
    EXIT.                      "ENCERRA A ESTRUTURA DE CONTROLE."
  ENDIF.                       "ENCERRA A ESTRUTURA DE CONTROLE."
    CONTADOR4 = CONTADOR4 + 1. "INCREMENTA O CONTADOR."
ENDDO.                         "ENCERRA ESTRUTURA DE CONTROLE."

"-------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
"[ESTRUTURA DE CONTROLE DE REPETIÇÃO COM USO DA INSTRUÇÃO CONTINUE.]"

DATA: LT_INT TYPE TABLE OF I,     "TABELA INTERNA DE INTEIROS."
      LS_INT LIKE LINE OF LT_INT, "ESTRUTURA DO TIPO DA TABELA INTERNA."
      LV_SUM TYPE I.              "VARIÁVEL DE NÚMERO INTEIRO."

LT_INT = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ). "PREENCHE A TABELA INTERNA COM NÚMEROS."

LOOP AT LT_INT INTO LS_INT. "ITERA SOBRE A TABELA INTERNA."
  
  IF LS_INT EQ 3.           "SE FOR ENCONTRADO UM VALOR IGUAL A 3 NA TABELA...
    CONTINUE.               "IGNORA A LINHA REFERENTE AO VALOR E PULA PARA A PRÓXIMA."
  ENDIF.                    "ENCERRA ESTRUTURA DE CONTROLE."

  LV_SUM = LV_SUM + LS_INT. "INCREMENTA A VARIÁVEL COM OS NÚMEROS DA TABELA."

ENDLOOP.                    "ENCERRA ESTRUTURA DE CONTROLE."

WRITE LV_SUM.               "EXIBE A MENSAGEM."

"-------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
"[LOOPS ANINHADOS.]"

DATA: LT_OUT TYPE TABLE OF I, "TABELA INTERNA DE INTEIROS."
      LT_IN TYPE TABLE OF I,  "TABELA INTERNA DE INTEIROS."
      LV_SUM2 TYPE I.         "VARIÁVEL DE INTEIROS PARA CALCULAR A SOMA DOS NÚMEROS."

LT_OUT = VALUE #( ( 10 ) ( 20 ) ( 30 ) ). "PREENCHE A TABELA INTERNA COM NÚMEROS."
LT_IN = VALUE #( ( 1 ) ( 2 ) ( 3 ) ).     "PREENCHE A TABELA INTERNA COM NÚMEROS."

LOOP AT LT_OUT INTO DATA(LS_OUT).        "ITERA SOBRE A TABELA INTERNA."
  LOOP AT LT_IN INTO DATA(LS_IN).        "ITERA SOBRE A TABELA INTERNA."

    LV_SUM2 = LV_SUM2 + LS_OUT + LS_INT. "INCREMENTA A VARIÁVEL COM A SOMA DOS NÚMEROS DAS TABELAS."

    "O QUE ACONTECE AQUI DENTRO...
    "(1+10) + (1+20) + (1+30) + (2+10) + (2+20) + (2+30) + (3+10) + (3+20) + (3+30).

  ENDLOOP.                               "ENCERRA ESTRUTURA DE CONTROLE."
ENDLOOP.                                 "ENCERRA ESTRUTURA DE CONTROLE."

WRITE LV_SUM2.                           "EXIBE A MENSAGEM."

"-------------------------------------------------------------------------------------------------------------
*"------------------------------------------------------------------------------------------------------------
"EXERCÍCIO DE APOIO.

"CRIE UMA TABELA CHAMADA LT_STUDENTS COM AS COLUNAS: [ID],[NAME],[DISCIPLINE] AND [NOTE]."

*"------------------------------------------------------------------------------------------------------------

"01 - INSIRA DADOS À TABELA."

TYPES: BEGIN OF TY_STUDENTS, "CRIA A WORK AREA EM QUE MINHA TABELA VAI SE BASEAR."
  ID TYPE I,                 "ID DO ALUNO."
  NAME TYPE STRING,          "NOME DO ALUNO."
  DISCIPLINE TYPE STRING,    "DISCIPLINA DO ALUNO."
  NOTE TYPE I,               "NOTA DO ALUNO."
END OF TY_STUDENTS.          "ENCERRA A WORK AREA."

DATA LT_STUDENTS TYPE TABLE OF TY_STUDENTS. "TABELA INTERNA RECEBE OS CAMPOS DA ESTRUTURA COMO COLUNAS."

LT_STUDENTS = VALUE #( "INSERE VALORES À TABELA INTERNA."
  ( ID = 1 NAME = 'John' DISCIPLINE = 'Math' NOTE = 8 )
  ( ID = 2 NAME = 'Maria' DISCIPLINE = 'Portuguese' NOTE = 7 )
  ( ID = 3 NAME = 'Carlos' DISCIPLINE = 'Science' NOTE = 6 )
  ( ID = 4 NAME = 'Ana' DISCIPLINE = 'History' NOTE = 9 )
  ( ID = 5 NAME = 'Sophia' DISCIPLINE = 'Physics' NOTE = 5 )
  ( ID = 6 NAME = 'Pedro' DISCIPLINE = 'Chemistry' NOTE = 4 )
  ( ID = 7 NAME = 'Laura' DISCIPLINE = 'Biology' NOTE = 2 )
  ( ID = 8 NAME = 'Gabriel' DISCIPLINE = 'Geography' NOTE = 1 ) ).

*"------------------------------------------------------------------------------------------------------------

"02 - CALCULE A MÉDIA DAS NOTAS DOS ALUNOS."

DATA: LV_COUNT TYPE I,           "VARIÁVEL PARA CONTAR O NÚMERO DE LINHAS DA TABELA INTERNA."
      LV_SUM3 TYPE P DECIMALS 1, "VARIÁVEL PARA SER ALIMENTADA COM AS NOTAS."
      MEDIA TYPE P DECIMALS 1.   "MEDIA CARREGA A OPERACAO FINAL DO CÁLCULO."

LV_COUNT = 0. "INICIA A VARIÁVEL."

LOOP AT LT_STUDENTS INTO DATA(LS_STUDENTS). "ITERA SOBRE A TABELA INTERNA.
  LV_COUNT = LV_COUNT + 1.              "INCREMENTA A VARIÁVEL."
  LV_SUM3 = LV_SUM3 + LS_STUDENTS-NOTE. "ALIMENTA A VARIÁVEL COM AS NOTAS DOS ALUNOS."
ENDLOOP.                                "ENCERRA ESTRUTURA DE CONTROLE."

MEDIA = LV_SUM3 / LV_COUNT. "DEFINE OPERAÇÃO DE CÁLCULO DA MÉDIA."

WRITE: / 'A MÉDIA DOS ALUNOS É: ', MEDIA. "EXIBE A MÉDIA."

SKIP 1. "SALTA UMA LINHA."

*"------------------------------------------------------------------------------------------------------------

"03 - ORDENE OS ALUNOS PELAS NOTAS PARA MOSTRAR QUEM TEVE A NOTA MAIS ALTA E MAIS BAIXA.
"PS - SEM O USO DA INSTRUÇÃO SORT.

DATA: COUNT TYPE I,           "VARIÁVEL PARA CONTAR O NÚMERO DE LINHAS DE UMA TABELA."
      LV_START TYPE I,        "VARIÁVEL PARA SERVIR DE PONTO INICIAL DE CONTAGEM."
      LT_IDS TYPE TABLE OF I, "TABELA INTERNA DE INTEIRO QUE DEVE SERVIR COMO UM ARRAY."
      LV_TEMP TYPE I,         "VARIÁVEL AUXILIAR PARA RECEBER RESULTADOS E GUARDÁ-LOS PROVISORIAMENTE."
      LV_INDEX TYPE I.        "VALOR PARA INDICAR O INDEX DE UM ARRAY."

COUNT = 0. "VALOR INICIAL DA VARIÁVEL."

LOOP AT LT_STUDENTS INTO LS_STUDENTS. "LOOP PARA CONTAR AS LINHAS DA TABELA."
  COUNT = COUNT + 1.                  "INCREMENTA A VARIÁVEL TODAS A VEZ QUE CRUZA UMA LINHA DA TABELA."
ENDLOOP.                              "ENCERRA ESTRUTURA DE CONTROLE."

IF COUNT > 1.   "VERIFICA SE HÁ MAIS DE UM ELEMENTO PARA ORDENAR."
  LV_INDEX = 1. "O INDEX RECEBE O VALOR INICIAL DA VARIÁVEL."

LOOP AT LT_STUDENTS INTO LS_STUDENTS. "LOOP PARA ITERAR SOBRE TABELA INTERNA."
  APPEND LS_STUDENTS-NOTE TO LT_IDS.  "INSERE AS NOTAS DA TABELA NO ARRAY."
ENDLOOP.                              "ENCERRA ESTRUTURA DE CONTROLE."

"INÍCIO DO ALGORITMO BUBBLE SORT."
DO COUNT TIMES.        "CICLO DURA O TEMPO DA QUANTIDADE DE LINHAS DA TABELA."
  DO COUNT - 1 TIMES.  "CICLO DENTRO DO CICLO COM AJUSTE DE - 1 PARA NÃO SAIR DO INDEX."
    IF LT_IDS[ LV_INDEX ] < LT_IDS[ LV_INDEX + 1 ]. "SE O ELEMENTO DO ARRAY FOR MENOR QUE SEU SUCESSOR."
      LV_TEMP = LT_IDS[ LV_INDEX ].                 "VARIÁVEL AUXILIAR RECEBE O ELEMENTO."
      LT_IDS[ LV_INDEX ] = LT_IDS[ LV_INDEX + 1 ].  "ELEMENTO RECEBE O ELEMENTO SUCESSOR."
      LT_IDS[ LV_INDEX + 1 ] = LV_TEMP.             "ELEMENTO SUCESSOR RECEBE A VARIÁVEL AUXILIAR."
    ENDIF.                                          "ENCERRA ESTRUTURA DE CONTROLE."
    LV_INDEX = LV_INDEX + 1.                        "INCREMENTA O INDEX."
  ENDDO.                                            "ENCERRA ESTRUTURA DE CONTROLE."
  LV_INDEX = 1.                                     "RESETA O INDEX PARA O PRÓXIMO CICLO."
ENDDO.                                              "ENCERRA ESTRUTURA DE CONTROLE."

ELSE.  "CASO NÃO HAJA ELEMENTOS PARA COMPARAR, EXIBE A MENSAGEM."
  WRITE: / 'Apenas um elemento encontrado. Não há necessidade de ordenação.'.
ENDIF. "ENCERRA ESTRUTURA DE CONTROLE."

DATA: LT_NEWTABLE TYPE TABLE OF TY_STUDENTS, "NOVA TABELA INTERNA PARA RECEBER OS DADOS ORDENADOS."
      LS_NEWSTRUCT LIKE LINE OF LT_NEWTABLE. "NOVA ESTRUTURA PARA RECEBER OS DADOS ORDENADOS."

LV_INDEX = 1. "VARIÁVEL RESETADA."

DO COUNT TIMES. "CICLO DURA O TEMPO DA QUANTIDADE DE LINHAS DA TABELA."
  "ITERA SOBRE A TABELA INTERNA ONDE A NOTA FOR IGUAL VALOR DO ARRAY."
  LOOP AT LT_STUDENTS INTO DATA(LS_TEMP2) WHERE NOTE = LT_IDS[ LV_INDEX ].
    LS_NEWSTRUCT-ID = LS_TEMP2-ID.                 "PREENCHE NOVA ESTRUTURA."
    LS_NEWSTRUCT-NAME = LS_TEMP2-NAME.             "PREENCHE NOVA ESTRUTURA."
    LS_NEWSTRUCT-DISCIPLINE = LS_TEMP2-DISCIPLINE. "PREENCHE NOVA ESTRUTURA."
    LS_NEWSTRUCT-NOTE = LS_TEMP2-NOTE.             "PREENCHE NOVA ESTRUTURA."
    APPEND LS_NEWSTRUCT TO LT_NEWTABLE.            "NOVA TABELA INTERNA RECEBE A ESTRUTURA."
  ENDLOOP.                                         "ENCERRA ESTRUTURA DE CONTROLE."
  LV_INDEX = LV_INDEX + 1.                         "INCREMENTA O INDEX DO ARRAY."
ENDDO.                                             "ENCERRA ESTRUTURA DE CONTROLE."

LOOP AT LT_NEWTABLE INTO DATA(LS_TEMP3).           "ITERA SOBRE A NOVA TABELA PREENCHIDA."
  WRITE: / 'ID: ', LS_TEMP3-ID.                    "ESCREVE OS DADOS DA TABELA."
  WRITE: / 'NAME: ', LS_TEMP3-NAME.
  WRITE: / 'DISCIPLINE: ', LS_TEMP3-DISCIPLINE.
  WRITE: / 'NOTE: ', LS_TEMP3-NOTE.
ENDLOOP.                                           "ENCERRA ESTRUTURA DE CONTROLE."

SKIP 1. "SALTA UMA LINHA."
*"------------------------------------------------------------------------------------------------------------
"04 - ORDENE OS ALUNOS POR ORDEM ALFABÉTICA.
"PS - SEM O USO DA INSTRUÇÃO SORT.

DATA: COUNT2 TYPE I,                "VARIÁVEL PARA CONTAR O NÚMERO DE LINHAS DE UMA TABELA."
      LV_START2 TYPE I,             "VARIÁVEL PARA SERVIR DE PONTO INICIAL DE CONTAGEM."
      LT_IDS2 TYPE TABLE OF STRING, "TABELA INTERNA DE INTEIRO QUE DEVE SERVIR COMO UM ARRAY."
      LV_TEMP2 TYPE STRING,         "VARIÁVEL AUXILIAR PARA RECEBER RESULTADOS E GUARDÁ-LOS PROVISORIAMENTE."
      LV_INDEX2 TYPE I.             "VALOR PARA INDICAR O INDEX DE UM ARRAY."

COUNT2 = 0. "VALOR INICIAL DA VARIÁVEL."

LOOP AT LT_STUDENTS INTO LS_STUDENTS. "LOOP PARA CONTAR AS LINHAS DA TABELA."
  COUNT2 = COUNT2 + 1.                "INCREMENTA A VARIÁVEL TODAS A VEZ QUE CRUZA UMA LINHA DA TABELA."
ENDLOOP.                              "ENCERRA ESTRUTURA DE CONTROLE."

IF COUNT2 > 1.   "VERIFICA SE HÁ MAIS DE UM ELEMENTO PARA ORDENAR."
  LV_INDEX2 = 1. "O INDEX RECEBE O VALOR INICIAL DA VARIÁVEL."

LOOP AT LT_STUDENTS INTO LS_STUDENTS. "LOOP PARA ITERAR SOBRE TABELA INTERNA."
  APPEND LS_STUDENTS-NAME TO LT_IDS2. "INSERE AS NOTAS DA TABELA NO ARRAY."
ENDLOOP.                              "ENCERRA ESTRUTURA DE CONTROLE."

"INÍCIO DO ALGORITMO BUBBLE SORT."
DO COUNT2 TIMES.        "CICLO DURA O TEMPO DA QUANTIDADE DE LINHAS DA TABELA."
  DO COUNT2 - 1 TIMES.  "CICLO DENTRO DO CICLO COM AJUSTE DE - 1 PARA NÃO SAIR DO INDEX."
    IF LT_IDS2[ LV_INDEX2 ] > LT_IDS2[ LV_INDEX2 + 1 ]. "SE O ELEMENTO DO ARRAY FOR MENOR QUE SEU SUCESSOR."
      LV_TEMP2 = LT_IDS2[ LV_INDEX2 ].                  "VARIÁVEL AUXILIAR RECEBE O ELEMENTO."
      LT_IDS2[ LV_INDEX2 ] = LT_IDS2[ LV_INDEX2 + 1 ].  "ELEMENTO RECEBE O ELEMENTO SUCESSOR."
      LT_IDS2[ LV_INDEX2 + 1 ] = LV_TEMP2.              "ELEMENTO SUCESSOR RECEBE A VARIÁVEL AUXILIAR."
    ENDIF.                                              "ENCERRA ESTRUTURA DE CONTROLE."
    LV_INDEX2 = LV_INDEX2 + 1.                          "INCREMENTA O INDEX."
  ENDDO.                                                "ENCERRA ESTRUTURA DE CONTROLE."
  LV_INDEX2 = 1.                                        "RESETA O INDEX PARA O PRÓXIMO CICLO."
ENDDO.                                                  "ENCERRA ESTRUTURA DE CONTROLE."

ELSE.  "CASO NÃO HAJA ELEMENTOS PARA COMPARAR, EXIBE A MENSAGEM."
  WRITE: / 'Apenas um elemento encontrado. Não há necessidade de ordenação.'.
ENDIF. "ENCERRA ESTRUTURA DE CONTROLE."

DATA: LT_NEWTABLE2 TYPE TABLE OF TY_STUDENTS,  "NOVA TABELA INTERNA PARA RECEBER OS DADOS ORDENADOS."
      LS_NEWSTRUCT2 LIKE LINE OF LT_NEWTABLE2. "NOVA ESTRUTURA PARA RECEBER OS DADOS ORDENADOS."

LV_INDEX = 1. "VARIÁVEL RESETADA."

DO COUNT2 TIMES. "CICLO DURA O TEMPO DA QUANTIDADE DE LINHAS DA TABELA."
  "ITERA SOBRE A TABELA INTERNA ONDE A NOTA FOR IGUAL AO PRIMEIRO ÍNDICE DO ARRAY."
  LOOP AT LT_STUDENTS INTO DATA(LS_TEMP4) WHERE NAME = LT_IDS2[ LV_INDEX2 ].
    LS_NEWSTRUCT2-ID = LS_TEMP4-ID.                 "PREENCHE NOVA ESTRUTURA."
    LS_NEWSTRUCT2-NAME = LS_TEMP4-NAME.             "PREENCHE NOVA ESTRUTURA."
    LS_NEWSTRUCT2-DISCIPLINE = LS_TEMP4-DISCIPLINE. "PREENCHE NOVA ESTRUTURA."
    LS_NEWSTRUCT2-NOTE = LS_TEMP4-NOTE.             "PREENCHE NOVA ESTRUTURA."
    APPEND LS_NEWSTRUCT2 TO LT_NEWTABLE2.           "NOVA TABELA INTERNA RECEBE A ESTRUTURA."
  ENDLOOP.                                          "ENCERRA ESTRUTURA DE CONTROLE."
  LV_INDEX2 = LV_INDEX2 + 1.                        "INCREMENTA O INDEX."
ENDDO.                                              "ENCERRA ESTRUTURA DE CONTROLE."

LOOP AT LT_NEWTABLE2 INTO DATA(LS_TEMP5).       "ITERA SOBRE A NOVA TABELA PREENCHIDA."
  WRITE: / 'ID: ', LS_TEMP5-ID.                 "ESCREVE OS DADOS DA TABELA."
  WRITE: / 'NAME: ', LS_TEMP5-NAME.
  WRITE: / 'DISCIPLINE: ', LS_TEMP5-DISCIPLINE.
  WRITE: / 'NOTE: ', LS_TEMP5-NOTE.
ENDLOOP.                                        "ENCERRA ESTRUTURA DE CONTROLE."

*"------------------------------------------------------------------------------------------------------------
