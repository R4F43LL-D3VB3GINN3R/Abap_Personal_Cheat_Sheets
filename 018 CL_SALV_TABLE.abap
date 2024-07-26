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
