REPORT ztemp002.

" Declaração de tipos para a seleção de campos da tabela: Documento de vendas: dados de cabeçalho
TYPES: BEGIN OF ty_vbak,
  vbeln TYPE vbak-vbeln,   " Número do documento de vendas
  erdat TYPE vbak-erdat,   " Data de criação do documento
  erzet TYPE vbak-erzet,   " Hora de criação do documento
  ernam TYPE vbak-ernam,   " Nome do usuário que criou o documento
END OF ty_vbak.

" Declaração de variáveis para armazenar os cabeçalhos e campos do HTML
DATA: t_header TYPE STANDARD TABLE OF w3head WITH HEADER LINE,
      t_fields TYPE STANDARD TABLE OF w3fields WITH HEADER LINE.

" Declaração de variáveis de trabalho para cabeçalho
DATA: wa_header TYPE w3head,
      w_head    TYPE w3head.

" Declaração de variáveis para a tabela interna e HTML
DATA: lt_vbak TYPE TABLE OF ty_vbak,       " Tabela interna para armazenar os dados selecionados
      t_html   TYPE STANDARD TABLE OF w3html,  " Tabela interna para armazenar o HTML gerado
      lt_fcat TYPE lvc_t_fcat WITH HEADER LINE.  " Tabela interna para o catálogo de campos (field catalog)

" Início da seleção de dados
START-OF-SELECTION.
  " Seleção dos dados da tabela VBAP com base nos campos definidos na estrutura ty_vbak
  SELECT vbeln, erdat, erzet, ernam 
    FROM vbak 
    INTO CORRESPONDING FIELDS OF TABLE @lt_vbak 
    UP TO 30 ROWS.
END-OF-SELECTION.

" Configuração dos textos das colunas para o catálogo de campos
lt_fcat-coltext = 'Sales Doc'.  " Texto da coluna para o número do documento de vendas
APPEND lt_fcat.
lt_fcat-coltext = 'Data'.  " Texto da coluna para a data de criação
APPEND lt_fcat.
lt_fcat-coltext = 'Hour'.  " Texto da coluna para a hora de criação
APPEND lt_fcat.
lt_fcat-coltext = 'Username'.  " Texto da coluna para o nome do usuário
APPEND lt_fcat.

" Loop para preencher os cabeçalhos e as propriedades das colunas
LOOP AT lt_fcat.
  w_head-text = lt_fcat-coltext.

  " Função para popular o cabeçalho do HTML
  CALL FUNCTION 'WWW_ITAB_TO_HTML_HEADERS'
    EXPORTING
      field_nr = sy-tabix
      text     = w_head-text
      fgcolor  = 'black'
      bgcolor  = 'white'
    TABLES
      header   = t_header.

  " Função para popular as propriedades das colunas do HTML
  CALL FUNCTION 'WWW_ITAB_TO_HTML_LAYOUT'
    EXPORTING
      field_nr = sy-tabix
      size     = '3'
      fgcolor  = 'black'
      bgcolor  = 'white'
    TABLES
      fields   = t_fields.
ENDLOOP.

" Configuração do título do relatório
wa_header-text = 'report'.
wa_header-font = 'Arial'.
wa_header-size = '2'.

" Preparação do HTML a partir da tabela interna
REFRESH t_html.
CALL FUNCTION 'WWW_ITAB_TO_HTML'
  EXPORTING
    table_header = wa_header
  TABLES
    html         = t_html
    fields       = t_fields
    row_header   = t_header
    itable       = lt_vbak.

" Download do HTML gerado para um arquivo no sistema local
CALL FUNCTION 'GUI_DOWNLOAD'
  EXPORTING
    filename  = 'C:\Users\rafal\Desktop\table_display\file.txt'
  TABLES
    data_tab  = t_html.
