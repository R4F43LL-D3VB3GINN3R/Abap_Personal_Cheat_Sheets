*----------------------------------------------------------------------*
***INCLUDE ZCL_FILETEXT.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class CL_FILETEXT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS cl_filetext DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS:

        "cria txt
        generate_text
          IMPORTING
            lv_path TYPE string
          CHANGING
            docheader TYPE string,

        "carrega txt criado
        load_text
          IMPORTING
            lv_path TYPE string.

  PRIVATE SECTION.

    DATA: lv_linha  TYPE string,          "linha do txt
          lt_linhas TYPE TABLE OF string, "tabela que acomoda as linhas do txt.
          lv_count  TYPE i VALUE 0.       "contagem de registros encontrados na tabela.

    DATA: docheader TYPE string.   "cabeçalho para o documento
    DATA: user      TYPE sy-uname. "login do user logado no sistema

    "estrutura com campos selecionados da tabela: Mestre de clientes (parte geral)
    TYPES: BEGIN OF ty_kna1,
      kunnr TYPE kna1-kunnr,
      name1 TYPE kna1-name1,
      regio TYPE kna1-regio,
      telf1 TYPE kna1-telf1,
    END OF ty_kna1.

    DATA: lt_kna1 TYPE TABLE OF ty_kna1, "Tabela Interna.
          ls_kna1 TYPE ty_kna1.

    DATA: load_table TYPE TABLE OF string. "Tabela Interna para carregar arquivos

ENDCLASS.

CLASS cl_filetext IMPLEMENTATION.

    METHOD generate_text. "Método para a criação do arquivo txt.

      "---------------------------------------------------------------------------------------
      "título do documento

      DATA: lv_datenow    TYPE char10,   "Data Atual
            lv_hour       TYPE sy-uzeit, "Hora Atual
            lv_hour_str   TYPE string,   "Hora
            lv_minute_str TYPE string,   "Minuto
            lv_second_str TYPE string.   "Segundo

      DATA: lv_time_str TYPE string. "String para receber strings de tempo concatenadas

      lv_hour = sy-uzeit. "Variável recebe hora atual no sistema.

      " Separação da hora, minuto e segundo
      lv_hour_str   = lv_hour+0(2).
      lv_minute_str = lv_hour+2(2).
      lv_second_str = lv_hour+4(2).

      "Concatenando-os com ":" para formar o horário completo
      CONCATENATE lv_hour_str ':' lv_minute_str ':' lv_second_str INTO lv_time_str.

      "Função para formatar a data
      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
        EXPORTING
          date_internal = sy-datum
        IMPORTING
          date_external = lv_datenow.

      "Juntando o título, a data e a hora
      CONCATENATE docheader lv_datenow lv_time_str INTO docheader SEPARATED BY ' / '.

      "------------------------------------------------------------------------------------
      "extraindo dados da db para a tabela interna.
      SELECT kunnr,
             name1,
             regio,
             telf1 FROM kna1
             INTO CORRESPONDING FIELDS OF TABLE @lt_kna1
             WHERE regio = 'SP'.

      "Corrigindo campos vazios para telefone...
      LOOP AT lt_kna1 INTO ls_kna1.
        IF ls_kna1-telf1 IS INITIAL.
          ls_kna1-telf1 = '00000000000'.
          MODIFY lt_kna1 FROM ls_kna1.
        ENDIF.
      ENDLOOP.

      "-----------------------------------------------------------------------------------------
      "construcao do header

      lv_count = lines( lt_kna1 ). "contando os registros na tabela interna.
      DATA: lv_reg_count TYPE string,
            num TYPE string.

      num = lv_count.
      lv_reg_count = 'Contagem de Registros Encontrados:'.
      CONCATENATE lv_reg_count num INTO lv_reg_count SEPARATED BY ' '.

      user = sy-uname. "Define o user

      lv_linha = '--------------------------------------------------------'.
      APPEND lv_linha TO lt_linhas.
      lv_linha = docheader.
      APPEND lv_linha TO lt_linhas.
      lv_linha = lv_reg_count.
      APPEND lv_linha TO lt_linhas.
      CONCATENATE 'Enviado por: ' user INTO lv_linha SEPARATED BY ' '.
      APPEND lv_linha TO lt_linhas.
      lv_linha = '---------------------------------------------------------'.
      APPEND lv_linha TO lt_linhas.
      lv_linha = ' '.
      APPEND lv_linha TO lt_linhas.

      "construcao do corpo
      LOOP AT lt_kna1 INTO ls_kna1.
        lv_linha = '------------------------------'.
        APPEND lv_linha TO lt_linhas.
        CONCATENATE 'Número: ' ls_kna1-kunnr INTO lv_linha SEPARATED BY ' '.
        APPEND lv_linha TO lt_linhas.
        CONCATENATE 'Nome: ' ls_kna1-name1 INTO lv_linha SEPARATED BY ' '.
        APPEND lv_linha TO lt_linhas.
        CONCATENATE 'País: ' ls_kna1-regio INTO lv_linha SEPARATED BY ' '.
        APPEND lv_linha TO lt_linhas.
        CONCATENATE 'Phone: ' ls_kna1-telf1 INTO lv_linha SEPARATED BY ' '.
        APPEND lv_linha TO lt_linhas.
        lv_linha = '------------------------------'.
        APPEND lv_linha TO lt_linhas.
        lv_linha = ' '.
        APPEND lv_linha TO lt_linhas.
      ENDLOOP.

      "chamada da funcao para exportar o txt
      "a funcao irá receber a tabela
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename                        = lv_path
         filetype                         = 'ASC'
        TABLES
          data_tab                        = lt_linhas.

    ENDMETHOD.

    METHOD load_text. "método para carregar o arquivo e imprimir

      "função para carregar o arquivo.
      CALL FUNCTION 'GUI_UPLOAD'
        EXPORTING
          filename = lv_path     "caminho para o arquivo
        TABLES
          data_tab = load_table. "enxe a tabela com o conteúdo da tabela

        "imprime o conteúdo da tabela
        LOOP AT load_table INTO DATA(ls_table).
          IF ls_table IS NOT INITIAL.
            WRITE: / ls_table.
          ENDIF.
        ENDLOOP.

    ENDMETHOD.

ENDCLASS.
