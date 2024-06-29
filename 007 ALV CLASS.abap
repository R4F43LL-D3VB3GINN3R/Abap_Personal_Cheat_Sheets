*&---------------------------------------------------------------------*
*& Report ZTEMP001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztemp001.

CLASS lcl_salv_tab DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS : main IMPORTING
                           i_list   TYPE xfeld
                           i_grid   TYPE xfeld
                           i_alv_tb TYPE c,

      fetch_records,

      get_alv_instance IMPORTING
                         i_list   TYPE xfeld
                         i_grid   TYPE xfeld
                         i_alv_tb TYPE c,


      display_alv,

      color_column.

  PRIVATE SECTION.

    TYPE-POOLS : icon, sym.

    TYPES : BEGIN OF ty_flight,

              status TYPE c LENGTH 1,

              icon   TYPE icon_d. " char – 4

              INCLUDE TYPE sflight.

    TYPES : END OF ty_flight.


    CLASS-DATA : lt_flight          TYPE TABLE OF ty_flight,

                 ls_flight          TYPE ty_flight,

                 lo_salv_tab TYPE REF TO cl_salv_table,

                 lo_func     TYPE REF TO cl_salv_functions_list,

                 lo_cols     TYPE REF TO cl_salv_columns_table,

                 lo_col      TYPE REF TO cl_salv_column,


                 lo_col_icon TYPE REF TO cl_salv_column,

                 lo_icon     TYPE REF TO cl_salv_column_table,


                 rem_seat    TYPE i,

                 lt_icon     TYPE TABLE OF icon,

                 ls_icon     TYPE icon.

    CLASS-DATA:  ls_color TYPE lvc_s_colo.

ENDCLASS.



CLASS lcl_salv_tab IMPLEMENTATION.

  METHOD main.

    fetch_records( ).

    get_alv_instance( EXPORTING  i_list     = i_list
                                 i_grid     = i_grid
                                 i_alv_tb   = i_alv_tb ).


    display_alv( ).


  ENDMETHOD.


  METHOD fetch_records.

    DATA indx TYPE sy-tabix.

    DATA line  TYPE i.

SELECT  FROM sflight FIELDS * INTO CORRESPONDING FIELDS OF TABLE @LT_flight UP TO 40 ROWS.
    line = lines( lt_flight ).

    SELECT  FROM icon FIELDS * INTO TABLE @LT_icon UP TO @line ROWS.


    LOOP AT  lt_flight INTO ls_flight.

      indx = sy-tabix.

      rem_seat = ls_flight-seatsmax_b - ls_flight-seatsocc_b.

      IF  rem_seat = 0.

        ls_flight-status = 1 .

      ELSEIF rem_seat LE 10.

        ls_flight-status = 2.

      ELSE.

        ls_flight-status = 3.

      ENDIF.


      READ TABLE lt_icon INTO ls_icon INDEX indx .

      IF sy-subrc = 0.

        ls_flight-icon = ls_icon-id.

      ENDIF.


      IF indx <> 0.

        MODIFY lt_flight FROM ls_flight INDEX indx TRANSPORTING status icon.

      ENDIF.

      CLEAR ls_flight.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_alv_instance.

    DATA : flag.

    IF i_list = 'X' OR i_grid = 'X'.

      IF i_list = 'X'.

        flag = 'X'.

      ELSE.

        flag = ' '.

      ENDIF.

      TRY.

          CALL METHOD cl_salv_table=>factory
            EXPORTING
              list_display = flag
            IMPORTING
              r_salv_table = lo_salv_tab
            CHANGING
              t_table      = lt_flight.


          IF i_alv_tb = abap_true.

**Begin- Displaying toolbar on alv **

            CALL METHOD lo_salv_tab->get_functions
              RECEIVING
                value = lo_func.


            CALL METHOD lo_func->set_default
              EXPORTING
                value = if_salv_c_bool_sap=>true.  "displays toolbar on alv

**End- Displaying toolbar on alv**

          ENDIF.


** Begin – Hides particular column of the table in the list or grid**

          CALL METHOD lo_salv_tab->get_columns " get all cols of table
            RECEIVING
              value = lo_cols.

          TRY.

              CALL METHOD lo_cols->get_column  "get reference to particular  column
                EXPORTING
                  columnname = 'MANDT'         "hiding particular column
                RECEIVING
                  value      = lo_col.

              CALL METHOD lo_col->set_technical  " true hides the col on ui
                EXPORTING
                  value = if_salv_c_bool_sap=>true.


            CATCH cx_salv_not_found .

          ENDTRY.


** End – Hides particular column of the table in the list or grid**

** Begin – Set status field as traffic icon **

          TRY.

              CALL METHOD lo_cols->set_exception_column
                EXPORTING
                  value = 'STATUS'.

            CATCH cx_salv_data_error .

          ENDTRY.

**End – Set status field as traffic icon **


**Begin – Set icon for the column ICON**

          TRY.

              CALL METHOD lo_cols->get_column " get reference to particular column
                EXPORTING
                  columnname = 'ICON'
                RECEIVING
                  value      = lo_col_icon.

              lo_icon ?= lo_col_icon.

              CALL METHOD lo_icon->set_icon
                EXPORTING
                  value = if_salv_c_bool_sap=>true.

              CALL METHOD lo_icon->set_long_text
                EXPORTING
                  value = 'Icon'.


            CATCH cx_salv_not_found .

          ENDTRY.

**End – Set icon for the column ICON**


        CATCH cx_salv_msg .

      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD display_alv.

    color_column( ).

    CALL METHOD lo_salv_tab->display.

  ENDMETHOD.

  METHOD color_column.
    INCLUDE <color>.    "type pools
    DATA: lo_cols TYPE REF TO cl_salv_columns_table.
    DATA: lo_col TYPE REF TO cl_salv_column_table.
    lo_cols = lo_salv_tab->get_columns( ).
    lo_col ?= lo_cols->get_column( 'CURRENCY' ).   "for particular column color
    ls_color-col = col_positive.
    lo_col->set_color( ls_color ).
  ENDMETHOD.


ENDCLASS.


START-OF-SELECTION.


  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

    PARAMETERS : list     RADIOBUTTON GROUP g1,

                 grid     RADIOBUTTON GROUP g1,

                 alv_tool AS CHECKBOX.


  SELECTION-SCREEN END OF BLOCK b1.


  CALL METHOD lcl_salv_tab=>main
    EXPORTING
      i_list   = list
      i_grid   = grid
      i_alv_tb = alv_tool.
