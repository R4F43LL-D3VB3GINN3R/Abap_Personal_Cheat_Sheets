"--------------------------------------------------------------------------------------------"
[COLUNAS VERTICAIS]

SELECT FROM dd03l
  FIELDS *
    WHERE tabname = 'EKKO'
      INTO TABLE @DATA(it_ekko_str)
        up to 20 rows.

CALL FUNCTION 'GUI_DOWNLOAD'
  EXPORTING
    filename                        = 'C:\Users\rafal\Desktop\Rafael\new_file.xls'
  tables
    data_tab                        = it_ekko_str

"--------------------------------------------------------------------------------------------"
[COLUNAS HORIZONTAIS E COM CABEÇALHO]

REPORT ztemp002.

" Declaração de tipos para a seleção de campos da tabela: Documento de vendas: dados de cabeçalho
TYPES: BEGIN OF ty_vbak,
  vbeln TYPE vbak-vbeln,   " Número do documento de vendas
  erdat TYPE vbak-erdat,   " Data de criação do documento
  erzet TYPE vbak-erzet,   " Hora de criação do documento
  ernam TYPE vbak-ernam,   " Nome do usuário que criou o documento
END OF ty_vbak.

DATA: lt_vbak TYPE TABLE OF ty_vbak,       " Tabela interna para armazenar os dados selecionados
      lt_vbak_with_header TYPE TABLE OF ty_vbak, " Tabela interna para armazenar os dados com cabeçalho
      wa_vbak TYPE ty_vbak,                 " Estrutura de trabalho para armazenar os dados
      lt_fieldnames TYPE TABLE OF string.   " Tabela para armazenar os nomes dos campos

" Seleção dos dados da tabela VBAK com base nos campos definidos na estrutura ty_vbak
SELECT vbeln, erdat, erzet, ernam
  FROM vbak
  INTO TABLE @lt_vbak
  UP TO 30 ROWS.

" Adicionar os nomes dos campos à tabela lt_fieldnames
APPEND 'VBELN' TO lt_fieldnames.
APPEND 'ERDAT' TO lt_fieldnames.
APPEND 'ERZET' TO lt_fieldnames.
APPEND 'ERNAM' TO lt_fieldnames.

" Adicionar os nomes dos campos como a primeira linha da tabela lt_vbak_with_header
LOOP AT lt_fieldnames INTO DATA(lv_fieldname).
  ASSIGN COMPONENT lv_fieldname OF STRUCTURE wa_vbak TO FIELD-SYMBOL(<fs_field>).
  <fs_field> = lv_fieldname.
ENDLOOP.
APPEND wa_vbak TO lt_vbak_with_header.

" Adicionar os dados reais à tabela lt_vbak_with_header
APPEND LINES OF lt_vbak TO lt_vbak_with_header.

" Download do CSV gerado para um arquivo no sistema local
CALL FUNCTION 'GUI_DOWNLOAD'
  EXPORTING
    filename              = 'C:\Users\rafal\Desktop\file.xls'
    filetype              = 'ASC'
    write_field_separator = 'X'
  TABLES
    data_tab              = lt_vbak_with_header
  EXCEPTIONS
    file_write_error      = 1
    no_batch              = 2
    gui_refuse_filetransfer = 3
    invalid_type          = 4
    no_authority          = 5
    unknown_error         = 6
    header_not_allowed    = 7
    separator_not_allowed = 8
    filesize_not_allowed  = 9
    header_too_long       = 10
    dp_error_create       = 11
    dp_error_send         = 12
    dp_error_write        = 13
    unknown_dp_error      = 14
    access_denied         = 15
    dp_out_of_memory      = 16
    disk_full             = 17
    dp_timeout            = 18
    file_not_found        = 19
    dataprovider_exception = 20
    control_flush_error   = 21
    OTHERS                = 22.

IF sy-subrc <> 0.
  " Implement suitable error handling here
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
"--------------------------------------------------------------------------------------------"
[TABELA CUSTOMIZADA]

TYPES: BEGIN OF ty_empresas,
  bukrs TYPE t001-bukrs, "empresa
  butxt TYPE t001-butxt, "descricao empresa
END OF ty_empresas.

TYPES: BEGIN OF ty_compras,
  bukrs TYPE ekko-bukrs, "empresa
  aedat TYPE ekko-aedat, "data de criacao do registro
END OF ty_compras.

TYPES: BEGIN OF ty_output,
  bukrs TYPE t001-bukrs, "empresa
  butxt TYPE t001-butxt, "descricao empresa
  aedat TYPE ekko-aedat, "data de criacao do registro
END OF ty_output.

"tabelas internas
DATA: it_empresas    TYPE TABLE OF ty_empresas,
      it_compras     TYPE TABLE OF ty_compras,
      it_output      TYPE TABLE OF ty_output with header line,
      it_final_table TYPE TABLE OF ty_output with header line,
      it_fieldnames  TYPE TABLE OF string.

DATA: ls_output TYPE ty_output.

START-OF-SELECTION.

"preenchendo tabelas internas
SELECT bukrs, butxt FROM t001 INTO CORRESPONDING FIELDS OF TABLE @it_empresas WHERE land1 = 'BR'.
SELECT bukrs, aedat FROM ekko INTO CORRESPONDING FIELDS OF TABLE @it_compras FOR ALL ENTRIES IN @it_empresas WHERE bukrs = @it_empresas-bukrs.

"preenchendo tabela de saida
LOOP AT it_compras INTO DATA(ls_compras).
  READ TABLE it_empresas INTO DATA(ls_empresas) WITH KEY bukrs = ls_compras-bukrs.
    ls_output-bukrs = ls_empresas-bukrs.
    ls_output-butxt = ls_empresas-butxt.
    ls_output-aedat = ls_compras-aedat.
    APPEND ls_output TO it_output.
    CLEAR ls_output.
ENDLOOP.

"adicionando colunas
APPEND 'BUKRS' TO it_fieldnames.
APPEND 'BUTXT' TO it_fieldnames.
APPEND 'AEDAT' TO it_fieldnames.

"Inserindo a nova coluna na nova estrutura.
LOOP AT it_fieldnames INTO DATA(lv_fieldname).
  ASSIGN COMPONENT lv_fieldname OF STRUCTURE ls_output TO FIELD-SYMBOL(<fs_field>).
  <fs_field> = lv_fieldname.
ENDLOOP.
APPEND ls_output TO it_final_table.

APPEND LINES OF it_output TO it_final_table.

" Download do CSV gerado para um arquivo no sistema local
CALL FUNCTION 'GUI_DOWNLOAD'
  EXPORTING
    filename              = 'C:\Users\rafal\Desktop\file.xls'
    filetype              = 'ASC'
    write_field_separator = 'X'
  TABLES
    data_tab              = it_final_table
  EXCEPTIONS
    file_write_error      = 1
    no_batch              = 2
    gui_refuse_filetransfer = 3
    invalid_type          = 4
    no_authority          = 5
    unknown_error         = 6
    header_not_allowed    = 7
    separator_not_allowed = 8
    filesize_not_allowed  = 9
    header_too_long       = 10
    dp_error_create       = 11
    dp_error_send         = 12
    dp_error_write        = 13
    unknown_dp_error      = 14
    access_denied         = 15
    dp_out_of_memory      = 16
    disk_full             = 17
    dp_timeout            = 18
    file_not_found        = 19
    dataprovider_exception = 20
    control_flush_error   = 21
    OTHERS                = 22.

IF sy-subrc <> 0.
  " Implement suitable error handling here
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
"--------------------------------------------------------------------------------------------"
