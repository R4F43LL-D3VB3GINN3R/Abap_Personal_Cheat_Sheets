*&---------------------------------------------------------------------*
*& Report ZALV_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zalv_test.

DATA: it_spfli TYPE TABLE OF spfli.

"alv table
DATA: gr_table TYPE REF TO cl_salv_table.

"alv functions and grid
DATA: gr_functions TYPE REF TO cl_salv_functions,
      gr_display   TYPE REF TO cl_salv_display_settings.

"alv columns
DATA: gr_columns TYPE REF TO cl_salv_columns_table,
      gr_column  TYPE REF TO cl_salv_column_table,
      color      TYPE lvc_s_colo.

"alv sorts
DATA: gr_sorts TYPE REF TO cl_salv_sorts.

"aggregations
DATA: gr_agg TYPE REF TO cl_salv_aggregations.

"filters
DATA: gr_filter TYPE REF TO cl_salv_filters.

"layout
DATA: gr_layout TYPE REF TO cl_salv_layout,
      key       TYPE salv_s_layout_key.

START-OF-SELECTION.

  SELECT * FROM spfli INTO TABLE it_spfli UP TO 50 ROWS.

  cl_salv_table=>factory(
    EXPORTING
*        list_display   = if_salv_c_bool_sap=>false
      container_name = 'materials'
    IMPORTING
      r_salv_table   = gr_table
    CHANGING
      t_table        =    it_spfli
  ).
*    CATCH cx_salv_msg.

  "----------------------------------------------"
  "functions

  gr_functions = gr_table->get_functions( ).
  gr_functions->set_all( abap_true ).

  "----------------------------------------------"
  "features and titles

  gr_display = gr_table->get_display_settings( ).
  gr_display->set_striped_pattern( cl_salv_display_settings=>true ).
  gr_display->set_list_header( 'This is the heading' ).

  "----------------------------------------------"

  gr_columns = gr_table->get_columns( ).
  gr_column ?= gr_columns->get_column( 'CITYTO' ).

  "text columns size

  gr_column->set_long_text( 'This is long text' ).
  gr_column->set_medium_text( 'This is med text' ).
  gr_column->set_short_text( 'This is sh' ).

  "colors

  gr_column ?= gr_columns->get_column( 'CITYFROM' ).
  color-col = '6'.
  color-int = '1'.
  color-inv = '0'.

  gr_column->set_color( color ).

  "----------------------------------------------"
  "sorts

  gr_sorts = gr_table->get_sorts( ).
  gr_sorts->add_sort( columnname = 'CITYTO' subtotal = abap_true ).

  "----------------------------------------------"
  "aggregations

  gr_agg = gr_table->get_aggregations( ).
  gr_agg->add_aggregation( 'DISTANCE' ).

  "----------------------------------------------"
  "filters

  gr_filter = gr_table->get_filters( ).
  gr_filter->add_filter( columnname = 'CARRID' low = 'LH' ).

  "----------------------------------------------"
  "layout

  gr_layout = gr_table->get_layout( ).
  key-report = sy-repid.
  gr_layout->set_key( key ).
  gr_layout->set_save_restriction( cl_salv_layout=>restrict_none ).

  "----------------------------------------------"
  "display

  gr_table->display( ).

  "----------------------------------------------"


END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

**&---------------------------------------------------------------------*
**& Report ZALV_TEST004
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
REPORT zalv_test004.

TABLES: zempresa, zfuncionarios.
"tabela de empresas
DATA: it_empresa TYPE TABLE OF zempresa,
      ls_empresa TYPE zempresa.
*tabela de funcionarios
DATA: it_funcionario TYPE TABLE OF zfuncionarios,
      ls_funcionario TYPE zfuncionarios.
"estrutura do alv
TYPES: BEGIN OF wa_output,
         id_empresa       TYPE zempresa-id_empresa,
         nome             TYPE zempresa-nome,
         nome_funcionario TYPE zfuncionarios-nome_funcionario,
         status           TYPE icon_d,
       END OF wa_output.
"tabela e estrutura de saída
DATA: it_output TYPE TABLE OF wa_output,
      ls_output TYPE wa_output.
"alv basico
DATA: alv_empresa TYPE REF TO cl_salv_table.
"funcoes do alv
DATA: alv_functions TYPE REF TO cl_salv_functions,
      alv_settings  TYPE REF TO cl_salv_display_settings.
"colunas do alv
DATA: alv_columns TYPE REF TO cl_salv_columns_table,
      alv_column  TYPE REF TO cl_salv_column_table.
"ordenação do alv
DATA: alv_sorts TYPE REF TO cl_salv_sorts.
"filtros de alv
DATA: alv_filter TYPE REF TO cl_salv_filters.
"cores do alv
DATA: color1 TYPE lvc_s_colo.
"----------------------------------------------------------------------------
"preenche tabela interna

START-OF-SELECTION.
*  SELECT * FROM zempresa INTO TABLE it_empresa.
*  SELECT * FROM zfuncionarios INTO TABLE it_funcionario.
*
*  LOOP AT it_funcionario INTO ls_funcionario.
*    ls_output-id_empresa = ls_funcionario-id_empresa.
*    IF ls_output-id_empresa = '1'.
*      ls_output-status = icon_yellow_light.
*    ELSEIF ls_output-id_empresa = '2'.
*      ls_output-status = icon_green_light.
*    ELSE.
*      ls_output-status = icon_red_light.
*      ls_output-nome = ' '.
*    ENDIF.
*    ls_output-nome_funcionario = ls_funcionario-nome_funcionario.
*    LOOP AT it_empresa INTO ls_empresa WHERE id_empresa = ls_funcionario-id_empresa.
*      ls_output-nome = ls_empresa-nome.
*    ENDLOOP.
*    APPEND ls_output TO it_output.
*  ENDLOOP.

  "ou

  SELECT a~id_empresa
         a~nome
         b~nome_funcionario
      FROM zempresa AS a
      INNER JOIN zfuncionarios AS b ON a~id_empresa = b~id_empresa
      INTO ls_output.

    IF ls_output-id_empresa = '1'.
      ls_output-status = icon_yellow_light.
    ELSEIF ls_output-id_empresa = '2'.
      ls_output-status = icon_green_light.
    ELSE.
      ls_output-status = icon_red_light.
    ENDIF.

    APPEND ls_output TO it_output.
  ENDSELECT.
  "----------------------------------------------------------------------------
  "chamada do metodo para exibicao alv
  cl_salv_table=>factory(
    IMPORTING
      r_salv_table   = alv_empresa
    CHANGING
      t_table        = it_output ).
  "----------------------------------------------------------------------------
  "exibicao das funcionalidades do alv
  alv_functions = alv_empresa->get_functions( ).
  alv_functions->set_all( abap_true ).
  "exibicao das settings do alv
  alv_settings = alv_empresa->get_display_settings( ).
  alv_settings->set_striped_pattern( cl_salv_display_settings=>true ).
  alv_settings->set_list_header( 'Empresas / Funcionários' ).
  "----------------------------------------------------------------------------
  "colorindo colunas no alv.
  alv_columns = alv_empresa->get_columns( ).
  alv_column ?= alv_columns->get_column( 'NOME').
  color1-col = '1'.
  color1-int = '1'.
  color1-inv = '1'.
  alv_column->set_color( color1 ).
  "----------------------------------------------------------------------------
  "ordenação do alv
  alv_sorts = alv_empresa->get_sorts( ).
  alv_sorts->add_sort( columnname = 'ID_EMPRESA').

  alv_sorts = alv_empresa->get_sorts( ).
  alv_sorts->add_sort( columnname = 'NOME').
  "----------------------------------------------------------------------------
  "filtros do alv
  alv_filter = alv_empresa->get_filters( ).
  alv_filter->add_filter( columnname = 'ID_EMPRESA').

  alv_filter = alv_empresa->get_filters( ).
  alv_filter->add_filter( columnname = 'NOME').
  "----------------------------------------------------------------------------
  "hotspot alv
  alv_column ?= alv_columns->get_column( 'NOME' ).
  alv_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
  "----------------------------------------------------------------------------
  "otimizacao das colunas
  alv_empresa->get_columns( )->set_optimize( abap_true ).
  "----------------------------------------------------------------------------
  "exibicao do alv
  alv_empresa->display( ).
*"---------------------------------------------------------------------------
