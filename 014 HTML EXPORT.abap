*&---------------------------------------------------------------------*
*& Report ZTEMP002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztemp002.

*---------------------------------------------------------------------*
*          T Y P E S D E C L A R A T I O N
*---------------------------------------------------------------------*
"selecao de campos da tabela: Documento de vendas: dados de cabeçalho
TYPES: BEGIN OF ty_vbak,
  vbeln TYPE vbak-vbeln,
  erdat TYPE vbak-erdat,
  erzet TYPE vbak-erzet,
  ernam TYPE vbak-ernam,
END OF ty_vbak.

DATA: t_header TYPE STANDARD TABLE OF w3head   WITH HEADER LINE,
      t_fields TYPE STANDARD TABLE OF w3fields WITH HEADER LINE,
      t_html   TYPE STANDARD TABLE OF w3html,
      wa_header TYPE w3head,
      w_head    TYPE w3head.

DATA: lt_vbak TYPE TABLE OF ty_vbak,             
      lt_fcat TYPE lvc_t_fcat WITH HEADER LINE.

**---------------------------------------------------------------------*
**        S T A R T - O F - S E L E C T I O N
**---------------------------------------------------------------------*

START-OF-SELECTION.
SELECT vbeln, erdat, erzet, ernam FROM vbap INTO CORRESPONDING FIELDS OF TABLE @lt_vbak UP TO 30 ROWS.

**---------------------------------------------------------------------*
**        E N D - O F - S E L E C T I O N
**---------------------------------------------------------------------*
END-OF-SELECTION.

lt_fcat-coltext = 'Sales Doc'.
APPEND lt_fcat.
lt_fcat-coltext = 'Data'.
APPEND lt_fcat.
lt_fcat-coltext = 'Hour'.
APPEND lt_fcat.
lt_fcat-coltext = 'Username'.
APPEND lt_fcat.

LOOP AT lt_fcat.

  w_head-text = lt_fcat-coltext.

  "funcao para popular o header.
  CALL FUNCTION 'WWW_ITAB_TO_HTML_HEADERS'
   EXPORTING
     field_nr          = sy-tabix
     text              = w_head-text
     fgcolor           = 'black'
     bgcolor           = 'white'
   TABLES
     header            = t_header.

  "funcao para popular colunas
  CALL FUNCTION 'WWW_ITAB_TO_HTML_LAYOUT'
    EXPORTING
      field_nr        = sy-tabix
      size            = '3'
      fgcolor         = 'black'
      bgcolor         = 'white'
    TABLES
      fields          = t_fields.

ENDLOOP.

"titulo
wa_header-text = 'report'.
wa_header-font = 'Arial'.
wa_header-size = '2'.

"preparando html para tabela interna.
REFRESH t_html.
CALL FUNCTION 'WWW_ITAB_TO_HTML'
  EXPORTING
    table_header           = wa_header
  TABLES
    html                   = t_html
    fields                 = t_fields
    row_header             = t_header
    itable                 = lt_vbak.

"download do html
CALL FUNCTION 'GUI_DOWNLOAD'
  EXPORTING
    filename                        = 'C:\Users\rafal\Desktop\table_display\file.txt'
  TABLES
    data_tab                        = t_html.
