*&---------------------------------------------------------------------*
*& Report ZALV_TEST003
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zalv_test003.

"instancia da classe
DATA: o_table TYPE REF TO cl_salv_table.

"tabela interna
DATA: it_t001 TYPE TABLE OF t001.

"funcoes do alv
DATA: o_functions TYPE REF TO cl_salv_functions.

"classe de eventos
CLASS lcl_handle_events DEFINITION.
   PUBLIC SECTION.
    METHODS:
      on_user_command
      FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
ENDCLASS.
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
    CASE e_salv_function.
      WHEN 'MYFUNCTION'.
        MESSAGE 'Botão pressionado' TYPE 'I'.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

DATA: o_handler TYPE REF TO lcl_handle_events.
DATA: o_events TYPE REF TO cl_salv_events_table.

START-OF-SELECTION.

"preenche a tabela
SELECT * FROM t001 INTO TABLE it_t001.

*  TRY.
"metodo para exibir o alv
  CALL METHOD cl_salv_table=>factory
*    EXPORTING
*      list_display   = IF_SALV_C_BOOL_SAP=>FALSE
*      r_container    =
*      container_name =
    IMPORTING
      r_salv_table   = o_table "instancia da classe
    CHANGING
      t_table        = it_t001 "tabela interna
      .
*    CATCH cx_salv_msg.
*  ENDTRY.

  DATA: lo_cols TYPE REF TO cl_salv_columns,
        lo_col TYPE REF TO cl_salv_column.

  lo_cols = o_table->get_columns( ).

  TRY.
    lo_col = lo_cols->get_column( 'BUKRS' ).
    lo_col->set_long_text( '*Empresa*' ).
    lo_col->set_medium_text( '*Empresa*' ).
    lo_col->set_short_text( '*Empresa*' ).
  CATCH cx_salv_not_found.
  ENDTRY.

  o_functions = o_table->get_functions( ).
  o_functions->set_all( 'X' ).

o_table->set_screen_status(
  EXPORTING
    report        = sy-repid
    pfstatus      = 'SALV_STANDARD'
    set_functions = o_table->c_functions_all
).

o_events = o_table->get_event( ).

CREATE OBJECT o_handler.

SET HANDLER o_handler->on_user_command FOR o_events.

"metodo para exibir o alv
  o_table->display( ).
