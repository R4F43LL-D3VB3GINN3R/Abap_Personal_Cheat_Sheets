CLASS lcl_itab_to_excel DEFINITION.
  PUBLIC SECTION.
    METHODS:
      itab_to_xstring
        IMPORTING ir_data_ref TYPE REF TO data
        RETURNING VALUE(rv_xstring) TYPE xstring.
ENDCLASS.

CLASS lcl_itab_to_excel IMPLEMENTATION.
  METHOD itab_to_xstring.
    FIELD-SYMBOLS: <fs_data> TYPE ANY TABLE.
    CLEAR rv_xstring.
    ASSIGN ir_data_ref->* TO <fs_data>.

    TRY.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = DATA(lo_table)
          CHANGING  t_table      = <fs_data> ).

        DATA(lt_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                          r_columns      = lo_table->get_columns( )
                          r_aggregations = lo_table->get_aggregations( ) ).

        DATA(lo_result) = cl_salv_ex_util=>factory_result_data_table(
                            r_data         = ir_data_ref
                            t_fieldcatalog = lt_fcat ).

        cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
          EXPORTING
            xml_type       = if_salv_bs_xml=>c_type_xlsx
            xml_version    = cl_salv_bs_a_xml_base=>get_version( )
            r_result_data  = lo_result
            xml_flavour    = if_salv_bs_c_tt=>c_tt_xml_flavour_export
            gui_type       = if_salv_bs_xml=>c_gui_type_gui
          IMPORTING
            xml            = rv_xstring ).

      CATCH cx_root.
        CLEAR rv_xstring.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_send_oo_email DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS send_email.
ENDCLASS.

CLASS lcl_send_oo_email IMPLEMENTATION.
  METHOD send_email.
    "Get Data
    SELECT * FROM sflight INTO TABLE @DATA(lt_data).
    GET REFERENCE OF lt_data INTO DATA(lo_data_ref).

    DATA(lv_xstring) = NEW lcl_itab_to_excel( )->itab_to_xstring( lo_data_ref ).

    "Email code starts here
    TRY.
        "Create send request
        DATA(lo_send_request) = cl_bcs=>create_persistent( ).

        "Create mail body
        DATA(lt_body) = VALUE bcsy_text(
                          ( line = 'Dear Recipient,' )
                          ( )
                          ( line = 'PFA flight details file.' )
                          ( )
                          ( line = 'Thank You' ) ).

        "Set up document object
        DATA(lo_document) = cl_document_bcs=>create_document(
                              i_type    = 'RAW'
                              i_text    = lt_body
                              i_subject = 'Flight Details' ).

        "Add attachment
        lo_document->add_attachment(
          i_attachment_type    = 'xls'
          i_attachment_size    = CONV #( xstrlen( lv_xstring ) )
          i_attachment_subject = 'Flight Details'
          i_attachment_header  = VALUE #( ( line = 'Flights.xlsx' ) )
          i_att_content_hex    = cl_bcs_convert=>xstring_to_solix( lv_xstring ) ).

        "Add document to send request
        lo_send_request->set_document( lo_document ).

        "Set sender
        lo_send_request->set_sender(
          cl_cam_address_bcs=>create_internet_address(
            i_address_string = CONV #( 'sender@dummy.com' ) ) ).

        "Set Recipient
        lo_send_request->add_recipient(
          i_recipient = cl_cam_address_bcs=>create_internet_address(
                         i_address_string = CONV #( 'recipient@dummy.com' ) )
          i_express   = abap_true ).

        "Send Email
        DATA(lv_sent_to_all) = lo_send_request->send( ).
        COMMIT WORK.

      CATCH cx_send_req_bcs INTO DATA(lx_req_bsc).
        "Error handling
      CATCH cx_document_bcs INTO DATA(lx_doc_bcs).
        "Error handling
      CATCH cx_address_bcs INTO DATA(lx_add_bcs).
        "Error handling
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

"Main program to test
START-OF-SELECTION.
  DATA(o_mail) = NEW lcl_send_oo_email( ).
  o_mail->send_email( ).
