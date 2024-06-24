*&---------------------------------------------------------------------*
*& Include          ZSDR003_LCL
*&---------------------------------------------------------------------*

CLASS lcl_mov_ativos DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS start.

    METHODS get_data.

    METHODS save_data.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_refkey,
             refkey TYPE j_1brefkey,
           END OF ty_refkey,

           BEGIN OF ty_fname,
             fname TYPE lvc_fname,
           END OF ty_fname,

           BEGIN OF ty_srchlp,
             fname    TYPE lvc_fname,
             ddic_ref TYPE salv_s_ddic_reference,
           END OF ty_srchlp,

           BEGIN OF ty_equi_det,
             equipamento    TYPE zsdst001-num_equipamento,
             loc_instalacao TYPE zsdst001-local_instalacao,
*             horimentro     TYPE zsdst001-horimetro,
           END OF ty_equi_det,

           BEGIN OF ty_motivos,
             abgru TYPE tvag-abgru,
             bezei TYPE tvagt-bezei,
             conc  TYPE char44,
           END OF ty_motivos,

           BEGIN OF ty_fdata.
             INCLUDE TYPE zsdst001.
             TYPES flg_contract TYPE flag.
             TYPES flg_add_data TYPE flag.
           TYPES END OF ty_fdata.



    TYPES: gtt_refkey TYPE STANDARD TABLE OF ty_refkey WITH DEFAULT KEY,
           gtt_fname  TYPE STANDARD TABLE OF ty_fname WITH DEFAULT KEY,
           gtt_srchlp TYPE STANDARD TABLE OF ty_srchlp WITH DEFAULT KEY,
           gtt_sdt001 TYPE STANDARD TABLE OF zsdst001 WITH DEFAULT KEY,
           gtt_fdata  TYPE STANDARD TABLE OF ty_fdata WITH DEFAULT KEY,
           gtt_motiv  TYPE STANDARD TABLE OF ty_motivos WITH DEFAULT KEY.

    CONSTANTS: gc_sim_rem   TYPE auart VALUE 'ZSIM',
               gc_invoice   TYPE vbtypl_n VALUE 'M',
               gc_logo      TYPE bds_typeid VALUE 'LOGO_ARMAC',
               gc_update    TYPE c VALUE 'U',
               gc_separador TYPE c VALUE '-',
               gc_erro      TYPE c VALUE 'E',
               gc_contract  TYPE c VALUE 'G',
               gc_msg(8)    TYPE c VALUE 'ZMOVATIV'.

    DATA: gt_data     TYPE gtt_sdt001,
          gt_data_old TYPE gtt_sdt001,
          gs_data     TYPE zsdst001,
          gt_motivos  TYPE gtt_motiv.

    DATA: go_salv TYPE REF TO cl_salv_table.

    METHODS create_alv.

    METHODS enable_fields.

    METHODS set_fields_not_visible.

    METHODS set_functions.

    METHODS set_header.

    METHODS set_dropdown.

    METHODS update_cost_center FOR EVENT added_function OF cl_salv_events_table IMPORTING e_salv_function.

    METHODS call_bapi_cust_contract IMPORTING it_selected_rows TYPE gtt_fdata.

    METHODS get_equipment_detail IMPORTING ev_charg TYPE zsdst001-lote RETURNING VALUE(rs_equipment) TYPE ty_equi_det.

    METHODS get_selected_rows RETURNING VALUE(rt_selected_rows) TYPE gtt_fdata.

    METHODS validate_row IMPORTING is_row     TYPE zsdst001
                                   is_row_old TYPE zsdst001
                         EXPORTING es_msg     TYPE esp1_message_wa_type
                                   es_flg_ctc TYPE c
                                   es_flg_add TYPE c.

ENDCLASS.

DATA(go_mov_ativos) = NEW lcl_mov_ativos(  ).
DATA go_container TYPE REF TO cl_gui_custom_container.

CLASS lcl_mov_ativos IMPLEMENTATION.

  METHOD get_data.

    DATA lv_handle TYPE i.

    SELECT
        vbak~vbeln,
        vbap~posnr,
        vbak~kunnr,
        kna1~name1,
        kna1~name4,
        vbap~werks,
        vbap~matnr,
        vbap~charg,
        vbap~abgru,
        vbap~zmeng,
        vbap~prctr,
        vbak~erdat
        FROM vbak INNER JOIN vbap ON vbak~vbeln = vbap~vbeln
        LEFT JOIN kna1 ON vbak~kunnr = kna1~kunnr
        INTO TABLE @DATA(lt_contrato)
        WHERE vbak~vbeln IN @s_vbeln
          AND vbak~kunnr IN @s_kunnr
          AND vbak~erdat IN @s_erdat
          AND vbap~charg IN @s_charg
          AND vbak~ernam IN @s_ernam
          AND vbak~vbtyp = @gc_contract.

*            s_equip TYPE equi-equnr,
*            s_local TYPE iflot-tplnr.

    IF lines( lt_contrato ) > 0.

      SELECT
          veda~vbeln,
          veda~vposn,
          veda~vbegdat,
          veda~venddat
          FROM veda
          INTO TABLE @DATA(lt_datas)
          FOR ALL ENTRIES IN @lt_contrato
          WHERE vbeln = @lt_contrato-vbeln.

      SELECT
          vbak~vbeln,
          vbap~posnr,
          vbap~matnr,
          vbap~charg,
          vbak~vgbel,
          vbak~auart
          FROM vbak INNER JOIN vbap ON vbak~vbeln = vbap~vbeln
          INTO TABLE @DATA(lt_ordem_sr)
          FOR ALL ENTRIES IN @lt_contrato
          WHERE vbak~vgbel = @lt_contrato-vbeln
            AND vbak~auart = @gc_sim_rem
            AND vbap~matnr = @lt_contrato-matnr
            AND vbap~charg = @lt_contrato-charg.

      SELECT
          vbfa~vbeln,
          vbfa~vbelv,
          vbfa~vbtyp_n
          FROM vbfa INNER JOIN vbrk ON vbfa~vbeln = vbrk~vbeln
          INTO TABLE @DATA(lt_fatura_sr)
          FOR ALL ENTRIES IN @lt_ordem_sr
          WHERE vbfa~vbelv   = @lt_ordem_sr-vbeln
            AND vbfa~vbtyp_n = @gc_invoice
            AND vbrk~fksto   = @space.

      DATA(lt_fatura_aux) = VALUE gtt_refkey( FOR ls_refkey IN lt_fatura_sr ( refkey = ls_refkey-vbeln ) ).

      IF lines( lt_fatura_sr ) > 0.

        SELECT
            j_1bnflin~docnum,
            j_1bnflin~refkey,
            j_1bnfdoc~nfenum
            FROM j_1bnflin INNER JOIN j_1bnfdoc ON j_1bnflin~docnum = j_1bnfdoc~docnum
            INTO TABLE @DATA(lt_nf_envio)
            FOR ALL ENTRIES IN @lt_fatura_aux
            WHERE j_1bnflin~refkey = @lt_fatura_aux-refkey.


        SELECT
            vbak~vbeln,
            vbak~vgbel,
            vbak~xblnr
            FROM vbak
            INTO TABLE @DATA(lt_ordem_dev)
            FOR ALL ENTRIES IN @lt_fatura_sr
            WHERE vbak~vgbel = @lt_fatura_sr-vbeln.

      ENDIF.

      SELECT
          vbeln,
          posnr,
          link_pipefy,
          fast_field,
          maquina_cliente,
          observacao,
          horimetro_inicio,
          horimetro_fim,
          local_instalacao,
          centro_lucro
          FROM zsdt001
          INTO TABLE @DATA(lt_add_data)
          FOR ALL ENTRIES IN @lt_contrato
          WHERE vbeln = @lt_contrato-vbeln
            AND posnr = @lt_contrato-posnr.

      SELECT a~abgru,
             b~bezei
        FROM tvag AS a INNER JOIN tvagt AS b
         ON a~abgru = b~abgru
        INTO TABLE @DATA(lt_abgru)
        WHERE spras = @sy-langu.

      gt_motivos = VALUE gtt_motiv( FOR ls_motivo IN lt_abgru ( abgru  = ls_motivo-abgru
                                                                bezei  = ls_motivo-bezei
                                                                conc   = |{ ls_motivo-abgru } - | & |{ ls_motivo-bezei }| ) ).

    ENDIF.


    LOOP AT lt_contrato INTO DATA(ls_contrato).

      gs_data-contrato            = ls_contrato-vbeln.
      gs_data-num_item            = ls_contrato-posnr.
      gs_data-cod_cliente         = ls_contrato-kunnr.
      gs_data-nome_cliente        = ls_contrato-name1.
      gs_data-nome_centro         = ls_contrato-name4.
      gs_data-data_criacao        = ls_contrato-erdat.
      gs_data-centro              = ls_contrato-werks.
      gs_data-material            = ls_contrato-matnr.
      gs_data-lote                = ls_contrato-charg.
      gs_data-quantidade_prevista = ls_contrato-zmeng.
      gs_data-motivo_recusas      = VALUE #( gt_motivos[ abgru = ls_contrato-abgru ]-conc OPTIONAL ).

      DATA(ls_datas) = VALUE #( lt_datas[ vbeln = ls_contrato-vbeln vposn = ls_contrato-posnr ] OPTIONAL ).

      IF ls_datas IS INITIAL.
        ls_datas = VALUE #( lt_datas[ vbeln = ls_contrato-vbeln ] OPTIONAL ).
      ENDIF.

      gs_data-data_inicio = ls_datas-vbegdat.
      gs_data-data_fim    = ls_datas-venddat.

      IF gs_data-data_inicio IS INITIAL.
        gs_data-data_inicio = ls_contrato-erdat.
      ENDIF.

      DATA(ls_add_data) = VALUE #( lt_add_data[ vbeln = ls_contrato-vbeln posnr = ls_contrato-posnr ] OPTIONAL ).

      gs_data-link_pipefy        = ls_add_data-link_pipefy.
      gs_data-fast_field         = ls_add_data-fast_field.
      gs_data-maquina_no_cliente = ls_add_data-maquina_cliente.
      gs_data-observacao         = ls_add_data-observacao.


      gs_data-ordem_sim_rem = VALUE #( lt_ordem_sr[ vgbel = ls_contrato-vbeln
                                                    matnr = ls_contrato-matnr
                                                    charg = ls_contrato-charg ]-vbeln OPTIONAL ).

      gs_data-fatura_sim_rem = VALUE #( lt_fatura_sr[ vbelv = gs_data-ordem_sim_rem ]-vbeln OPTIONAL ).
      gs_data-nf_envio = VALUE #( lt_nf_envio[ refkey = gs_data-fatura_sim_rem ]-nfenum OPTIONAL ).

      DATA(ls_ordem_dev) = VALUE #( lt_ordem_dev[ vgbel = gs_data-fatura_sim_rem ] OPTIONAL ).
      gs_data-ordem_devolucao =  ls_ordem_dev-vbeln.
      gs_data-nf_devolucao = ls_ordem_dev-xblnr.

*      DATA(ls_equipment) = me->get_equipment_detail( ls_contrato-charg ).

      gs_data-num_equipamento  = ls_contrato-charg.
      gs_data-local_instalacao = ls_add_data-local_instalacao.
      gs_data-horimetro_inicio = ls_add_data-horimetro_inicio.
      gs_data-horimetro_fim    = ls_add_data-horimetro_fim.
      gs_data-imobilizado      = replace( val = ls_contrato-charg sub = gc_separador with = space occ = 1 ).
*      gs_data-centro_lucro     = COND prctr( WHEN ls_add_data-centro_lucro IS NOT INITIAL
*                                                  THEN ls_add_data-centro_lucro
*                                                  ELSE  |{ ls_contrato-name4 }| & |{ '*' }| ).
      gs_data-centro_lucro     = COND prctr( WHEN ls_contrato-prctr EQ 'ARM3'
                                                  THEN |{ ls_contrato-name4 }| & |{ '*' }|
                                                  ELSE ls_contrato-prctr  ).

      lv_handle = sy-tabix.

      APPEND VALUE salv_s_int4_column( columnname = 'MOTIVO_RECUSAS'
                                       value      = CONV i( sy-tabix ) ) TO gs_data-cell_type.

*  centro_lucro_dev ???

      APPEND gs_data TO gt_data.

      CLEAR: gs_data,
             ls_datas,
             ls_add_data,
             ls_ordem_dev.

    ENDLOOP.

    SORT gt_data BY contrato num_item.

    gt_data_old = gt_data.

    IF gt_data IS INITIAL.
      MESSAGE s000(zmovativ) DISPLAY LIKE gc_erro.
      LEAVE LIST-PROCESSING.
    ELSE.

      IF sy-dynnr <> '9000'.
        CALL SCREEN 9000.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD create_alv.

    TRY.

        IF go_container IS NOT BOUND.
          go_container = NEW cl_gui_custom_container( container_name = 'CONTAINER' ).
        ENDIF.

        cl_salv_table=>factory(  EXPORTING
*                                   list_display   = abap_false
                                   r_container    = go_container
                                   container_name = 'CONTAINER'
                                 IMPORTING
                                   r_salv_table   = go_salv
                                 CHANGING
                                   t_table        = gt_data ).

        go_salv->get_columns( )->set_optimize( abap_true ).

      CATCH cx_root.

    ENDTRY.

  ENDMETHOD.

  METHOD enable_fields.

    TRY.

        DATA(ls_edit) = go_salv->extended_grid_api( )->editable_restricted( ).

        DATA(lt_enable) = VALUE gtt_fname( ( fname = 'DATA_INICIO' )
                                           ( fname = 'DATA_FIM' )
                                           ( fname = 'MATERIAL' )
                                           ( fname = 'LOTE' )
                                           ( fname = 'MOTIVO_RECUSAS' )
                                           ( fname = 'LINK_PIPEFY' )
                                           ( fname = 'FAST_FIELD' )
                                           ( fname = 'LOCAL_INSTALACAO' )
                                           ( fname = 'HORIMETRO_INICIO' )
                                           ( fname = 'HORIMETRO_FIM' )
                                           ( fname = 'CENTRO_LUCRO' )
                                           ( fname = 'MAQUINA_NO_CLIENTE' )
                                           ( fname = 'OBSERVACAO' )
                                           ( fname = 'DROPDOWN' ) ).

        LOOP AT lt_enable INTO DATA(ls_enable).

          ls_edit->set_attributes_for_columnname(
                  EXPORTING
                   columnname = ls_enable-fname
                   all_cells_input_enabled = abap_true
                 ).

        ENDLOOP.

      CATCH cx_root.
        "handle exception
    ENDTRY.

  ENDMETHOD.

  METHOD set_fields_not_visible.

    TRY.

        DATA(lt_notvisibile) = VALUE gtt_fname( ( fname = 'QUANTIDADE_PREVISTA' )
                                                ( fname = 'NOME_CENTRO' ) ) .

        DATA(lo_columns) = go_salv->get_columns( ).

        LOOP AT lt_notvisibile INTO DATA(ls_notvisibile).

          DATA(lo_column) = lo_columns->get_column( columnname = ls_notvisibile-fname ).

          lo_column->set_visible( value = space ).

        ENDLOOP.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD set_functions.

    TRY.

        go_salv->get_functions( )->set_all( abap_true ).
        SET HANDLER update_cost_center FOR go_salv->get_event( ).

        go_salv->get_functions( )->add_function(
                    name = 'ATTCC'
                    text = 'Atualizar Centro de Lucro'
                    tooltip = 'Atualizar todos os itens do contrato com o centro de lucro selecionado.'
                    position = if_salv_c_function_position=>right_of_salv_functions ).


      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD set_header.

    TRY .

        DATA(lo_grid) = NEW cl_salv_form_layout_grid( ).
        DATA(lo_logo) = NEW cl_salv_form_layout_logo( ).

        lo_grid->create_header_information( row = 1  column = 2
                                            text = sy-title ).
*      Italico
        lo_grid->create_action_information( row = 2 column = 1 text = |{ TEXT-001 } { lines(  gt_data ) }| ).

        lo_grid->create_action_information( row = 3 column = 1 text = |{ TEXT-002 } { sy-datum DATE = USER } - { sy-uzeit TIME = USER }| ).

        lo_grid->create_action_information( row = 4 column = 1 text = |{ TEXT-001 } { sy-uname }| ).

*      Regular
*      lr_grid->create_text( row = 4 column = 1 text = |{ text-001 } { sy-uname }| ).

*      Negrito
*      lr_grid->create_label( row = 4 column = 1 text = |{ text-001 } { sy-uname }| ).

*     Logo criado no tcode OAER
*      Class Name: Picture
*      Class Type: OT
*      Object Key: LOGO_ARMAC
        lo_logo->set_left_content( lo_grid ).
        lo_logo->set_right_logo( gc_logo ).

        go_salv->set_top_of_list( lo_logo ).

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD set_dropdown.

    DATA: lo_dropdowns           TYPE REF TO cl_salv_dropdowns,
          lt_values              TYPE salv_t_value,
          ls_values              LIKE LINE OF lt_values,
          lo_functional_settings TYPE REF TO cl_salv_functional_settings,
          lo_column              TYPE REF TO cl_salv_column_table.


    DATA(lo_columns) = go_salv->get_columns( ).

    TRY.
        lo_column ?= lo_columns->get_column( 'MOTIVO_RECUSAS' ).
*        lo_column->set_technical( if_salv_c_bool_sap=>true ).
        lo_column->set_cell_type( if_salv_c_cell_type=>dropdown ).
        lo_column->set_dropdown_entry( 1 ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lo_columns->set_dropdown_entry_column( 'CELL_TYPE' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.

    lo_functional_settings = go_salv->get_functional_settings( ).
    lo_dropdowns = lo_functional_settings->get_dropdowns( ).

    lt_values = VALUE #( FOR ls_motivo IN gt_motivos (  ls_motivo-conc  ) ).
    APPEND space TO lt_values.
*    APPEND VALUE #( 'vamos'  ) TO lt_values. LVC_VALUE

    TRY.
        lo_dropdowns->add_dropdown(
          handle   = 1
          t_values = lt_values ).
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.
*<<<  YI3K307540

*    TRY.
*        lr_dropdowns->add_dropdown(
*          handle   = 2
*          t_values = lt_values ).
*      CATCH cx_salv_existing.                           "#EC NO_HANDLER
*    ENDTRY.
*
*    TRY.
*        lr_dropdowns->add_dropdown(
*          handle   = 3
*          t_values = lt_values ).
*      CATCH cx_salv_existing.                           "#EC NO_HANDLER
*    ENDTRY.

  ENDMETHOD.

  METHOD start.

*    me->get_data(  ).

    IF go_salv IS NOT BOUND.

      me->create_alv(  ).

      me->enable_fields(  ).

      me->set_functions(  ).

      me->set_fields_not_visible(  ).

      me->set_dropdown(  ).

      me->set_functions(  ).

      me->set_header(  ).

      go_salv->display( ).

    ENDIF.

  ENDMETHOD.

  METHOD save_data.

    go_salv->extended_grid_api( )->editable_restricted( )->validate_changed_data(
              IMPORTING
                is_input_data_valid = DATA(lv_valid) ).

    IF lv_valid IS NOT INITIAL.

      me->call_bapi_cust_contract( me->get_selected_rows( ) ).

    ENDIF.

  ENDMETHOD.

  METHOD call_bapi_cust_contract.

    DATA: lt_return TYPE TABLE OF bapiret2,
          ls_header TYPE bapisdh1,
          lt_item   TYPE TABLE OF bapisditm,
          lt_itemx  TYPE TABLE OF bapisditmx,
          lt_data   TYPE TABLE OF bapictr,
          lt_datax  TYPE TABLE OF bapictrx,
          lt_sd001  TYPE TABLE OF zsdt001,
          lt_msgs   TYPE esp1_message_tab_type,
          lv_ilocal TYPE string,
          lv_rfrsh  TYPE c.

    LOOP AT it_selected_rows INTO DATA(ls_data) GROUP BY ( contrato = ls_data-contrato ) ASCENDING
                                                        ASSIGNING FIELD-SYMBOL(<contrato>).

      LOOP AT GROUP <contrato> ASSIGNING FIELD-SYMBOL(<ls_data>).

        IF <ls_data>-flg_contract IS NOT INITIAL.

          APPEND VALUE bapisditm( itm_number = <ls_data>-num_item
                                  material   = <ls_data>-material
                                  batch      = <ls_data>-lote
                                  target_qty = <ls_data>-quantidade_prevista
                                  profit_ctr = <ls_data>-centro_lucro
                                  reason_rej = COND #( WHEN strlen( <ls_data>-motivo_recusas ) > 1 THEN substring( val = <ls_data>-motivo_recusas off = 0 len = 2 ) ELSE space ) ) TO lt_item.

          APPEND VALUE bapisditmx( updateflag = gc_update
                                   itm_number = <ls_data>-num_item
                                   material   = abap_true
                                   batch      = abap_true
                                   profit_ctr = abap_true
                                   reason_rej = abap_true ) TO lt_itemx.

          APPEND VALUE bapictr( itm_number = <ls_data>-num_item
                                con_st_dat = <ls_data>-data_inicio
                                con_en_dat = <ls_data>-data_fim ) TO lt_data.

          APPEND VALUE bapictrx( itm_number = <ls_data>-num_item
                                 con_st_dat = abap_true
                                 con_en_dat = abap_true  ) TO lt_datax.

          CLEAR lv_ilocal.

          CALL FUNCTION 'CONVERSION_EXIT_TPLNR_INPUT'
            EXPORTING
              input                = <ls_data>-local_instalacao
              i_flg_check_internal = abap_true
            IMPORTING
              output               = lv_ilocal
            EXCEPTIONS
              not_found            = 1
              OTHERS               = 2.

          DATA(ls_header_inx) = VALUE bapisdh1x( updateflag = gc_update ).

          lv_rfrsh = abap_true.

        ENDIF.

        IF <ls_data>-flg_add_data IS NOT INITIAL.

          APPEND VALUE zsdt001( vbeln            = <ls_data>-contrato
                                posnr            = <ls_data>-num_item
                                link_pipefy      = <ls_data>-link_pipefy
                                fast_field       = <ls_data>-fast_field
                                maquina_cliente  = <ls_data>-maquina_no_cliente
                                observacao       = <ls_data>-observacao
                                horimetro_inicio = <ls_data>-horimetro_inicio
                                horimetro_fim    = <ls_data>-horimetro_fim
                                local_instalacao = lv_ilocal
                                centro_lucro     = <ls_data>-centro_lucro ) TO lt_sd001.

          lv_rfrsh = abap_true.

        ENDIF.

      ENDLOOP.

      CALL FUNCTION 'BAPI_CUSTOMERCONTRACT_CHANGE'
        EXPORTING
          salesdocument       = CONV bapivbeln-vbeln( <ls_data>-contrato )
          contract_header_in  = ls_header
          contract_header_inx = ls_header_inx
        TABLES
          return              = lt_return
          contract_item_in    = lt_item
          contract_item_inx   = lt_itemx
          contract_data_in    = lt_data
          contract_data_inx   = lt_datax.

      REFRESH: lt_item,
               lt_itemx,
               lt_data,
               lt_datax.

      IF lines( lt_return ) > 0 .

        DELETE lt_return WHERE id = 'V4' AND number = '233'.

        DATA(lt_messages) = VALUE esp1_message_tab_type( FOR ls_return IN lt_return ( msgid  = ls_return-id
                                                                                      msgty  = ls_return-type
                                                                                      msgno  = ls_return-number
                                                                                      msgv1  = ls_return-message_v1
                                                                                      msgv2  = ls_return-message_v2
                                                                                      msgv3  = ls_return-message_v3
                                                                                      msgv4  = ls_return-message_v4
                                                                                      "lineno = sy-tabix
                                                                                      ) ).

        APPEND LINES OF lt_messages TO lt_msgs.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait          = abap_true.

      ENDIF.

    ENDLOOP.

    IF lines( lt_msgs ) > 0.

      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = lt_msgs.

    ENDIF.

    IF lines( lt_sd001 ) > 0.

      MODIFY zsdt001 FROM TABLE lt_sd001.

    ENDIF.

    IF lv_rfrsh = abap_true.

      REFRESH gt_data.

      me->get_data(  ).

      BREAK cast-alvaro.

      go_salv->refresh( ).

    ENDIF.

  ENDMETHOD.

  METHOD get_equipment_detail.

*    DATA: ls_specific    TYPE bapi_itob_eq_only,
*          ls_return      TYPE bapiret2,
*          ls_instalation TYPE bapi_itob_installation_details,
*          lt_measure     TYPE TABLE OF rihimrg.
*
*    CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
*      EXPORTING
*        equipment                       = CONV bapi_itob_parms-equipment( ev_charg )
*        request_installation_data       = abap_true
*     IMPORTING
*        data_specific_exp               = ls_specific
*        return                          = ls_return
*        data_installation               = ls_instalation.
*
*    IF sy-subrc IS INITIAL.
*
*      rs_equipment-equipamento = ls_specific-serialno.
*      rs_equipment-loc_instalacao = ls_instalation-installationlocation.
*
*    ENDIF.
*
*    CALL FUNCTION 'GET_MEASURING_DOCUMENTS'
*      EXPORTING
*        equipment        = CONV equnr( ev_charg )
*      tables
*        et_rihimrg       = lt_measure.
*
*
*    IF sy-subrc IS INITIAL.
*
*      rs_equipment-horimetro =
*
*    ENDIF.

  ENDMETHOD.

  METHOD get_selected_rows.

*      DATA(lt_rows) = VALUE salv_t_row( ( 2 ) ( 3 ) ).
*      rt_selected_rows = go_salv->get_selections( )->set_selected_rows( lt_rows ).
*      rt_selected_rows = VALUE gtt_sdt001( FOR ls_row IN lt_rows ( ( gt_data[ ls_row ] ) ).

    DATA: lt_msgs     TYPE esp1_message_tab_type,
          ls_msg      TYPE esp1_message_wa_type,
          ls_fdata    TYPE ty_fdata,
          lv_flag_ctc TYPE c,
          lv_flag_add TYPE c.

    DATA(lt_rows) = go_salv->get_selections( )->get_selected_rows( ).

    LOOP AT lt_rows INTO DATA(ls_rows).
      IF ( gt_data[ ls_rows ] ) <> ( gt_data_old[ ls_rows ] ).

        me->validate_row( EXPORTING is_row     = gt_data[ ls_rows ]
                                    is_row_old = gt_data_old[ ls_rows ]
                          IMPORTING es_msg     = ls_msg
                                    es_flg_ctc = lv_flag_ctc
                                    es_flg_add = lv_flag_add ).

        ls_msg-lineno = sy-tabix.
        IF ls_msg-msgv1 IS NOT INITIAL.
          APPEND ls_msg TO lt_msgs.
        ENDIF.

        IF ls_msg-msgno IS INITIAL.
          ls_fdata = CORRESPONDING #( gt_data[ ls_rows ] ).
          ls_fdata-flg_contract = lv_flag_ctc.
          ls_fdata-flg_add_data = lv_flag_add.
          APPEND ls_fdata TO rt_selected_rows.
        ELSE.
          MODIFY gt_data INDEX ls_rows FROM gt_data_old[ ls_rows ].
        ENDIF.

        CLEAR ls_fdata.

      ENDIF.
    ENDLOOP.

    IF lines( rt_selected_rows ) < 1.
      LOOP AT gt_data INTO DATA(ls_data).
        IF ( ls_data ) <> ( gt_data_old[ sy-tabix ] ).

          me->validate_row( EXPORTING is_row     = ls_data
                                      is_row_old = gt_data_old[ sy-tabix ]
                            IMPORTING es_msg     = ls_msg
                                      es_flg_ctc = lv_flag_ctc
                                      es_flg_add = lv_flag_add ).
          ls_msg-lineno = sy-tabix.
          IF ls_msg-msgv1 IS NOT INITIAL.
            APPEND ls_msg TO lt_msgs.
          ENDIF.

          IF ls_msg-msgno IS INITIAL.
            ls_fdata = CORRESPONDING #( gt_data[ sy-tabix ] ).
            ls_fdata-flg_contract = lv_flag_ctc.
            ls_fdata-flg_add_data = lv_flag_add.
            APPEND ls_fdata TO rt_selected_rows.
          ELSE.
            MODIFY gt_data INDEX sy-tabix FROM gt_data_old[ sy-tabix ].
          ENDIF.

          CLEAR ls_fdata.

        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lines( lt_msgs ) > 0.

      REFRESH rt_selected_rows.

      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = lt_msgs.

    ENDIF.

  ENDMETHOD.

  METHOD validate_row.

    CONSTANTS: gc_msg_obg TYPE char3 VALUE '001',
               gc_msg_sm  TYPE char3 VALUE '002',
               gc_msg_dev TYPE char3 VALUE '003',
               gc_msg_inv TYPE char3 VALUE '006'.

    es_msg-msgid = gc_msg.
    es_msg-msgty = gc_erro.

    es_flg_ctc = abap_true.
    es_flg_add = abap_true.

    "validar
    "ZSDT0001

    IF ( is_row-data_inicio = is_row_old-data_inicio
        AND is_row-data_fim = is_row_old-data_fim
        AND is_row-material = is_row_old-material
        AND is_row-lote = is_row_old-lote
        AND is_row-motivo_recusas = is_row_old-motivo_recusas
        AND is_row-centro_lucro = is_row_old-centro_lucro )
        AND is_row-fatura_sim_rem IS NOT INITIAL.

      CLEAR es_flg_ctc.

    ENDIF.

    IF is_row-link_pipefy = is_row_old-link_pipefy
      AND is_row-fast_field = is_row_old-fast_field
      AND is_row-maquina_no_cliente = is_row_old-maquina_no_cliente
      AND is_row-observacao = is_row_old-observacao
      AND is_row-local_instalacao = is_row_old-local_instalacao
      AND is_row-horimetro_inicio = is_row_old-horimetro_inicio
      AND is_row-horimetro_fim = is_row_old-horimetro_fim
      AND is_row-centro_lucro = is_row_old-centro_lucro.

      CLEAR es_flg_add.

    ENDIF.

    IF is_row-fatura_sim_rem IS INITIAL.

      IF is_row-data_inicio IS INITIAL.

        es_msg-msgno  = gc_msg_obg.
        es_msg-msgv1  = 'Data Início Contrato'.
        EXIT.

      ENDIF.

      IF is_row-data_fim IS INITIAL.

        es_msg-msgno  = gc_msg_obg.
        es_msg-msgv1  = 'Data Fim Contrato'.
        EXIT.

      ENDIF.

      IF is_row-material IS INITIAL.

        es_msg-msgno  = gc_msg_obg.
        es_msg-msgv1  = 'Material'.
        EXIT.

      ENDIF.

*      IF is_row-lote IS INITIAL
*        AND is_row-motivo_recusas IS INITIAL.
*
*        es_msg-msgno  = gc_msg_obg.
*        es_msg-msgv1  = 'Lote'.
*        EXIT.
*
*      ENDIF.

      IF is_row-local_instalacao IS INITIAL.

        es_msg-msgno  = gc_msg_obg.
        es_msg-msgv1  = 'Local de Instalação'.
        EXIT.

      ENDIF.

      IF is_row-horimetro_inicio IS INITIAL.

        es_msg-msgno  = gc_msg_obg.
        es_msg-msgv1  = 'Horímetro Inicial'.
        EXIT.

      ENDIF.

      IF is_row-centro_lucro IS INITIAL.

        es_msg-msgno  = gc_msg_obg.
        es_msg-msgv1  = 'Centro de Lucro'.
        EXIT.

      ELSE.

        SELECT * UP TO 1 ROWS
          FROM cepc
          INTO @DATA(ls_cepc)
          WHERE prctr = @is_row-centro_lucro.
        ENDSELECT.

        IF sy-subrc IS NOT INITIAL.

           es_msg-msgno  = gc_msg_inv.
           es_msg-msgv1  = 'Centro de Lucro'.
           EXIT.

        ELSE.

          IF ls_cepc-prctr+0(4) <> is_row-nome_centro+0(4).

             es_msg-msgno  = gc_msg_inv.
             es_msg-msgv1  = 'Centro de Lucro'.
             EXIT.

          ENDIF.

        ENDIF.

      ENDIF.

    ELSE.

      IF is_row-data_inicio <> is_row_old-data_inicio.

        es_msg-msgno  = gc_msg_sm.
        es_msg-msgv1  = 'Data Início Contrato'.
        EXIT.

      ENDIF.

      IF is_row-data_fim IS INITIAL.

        es_msg-msgno  = gc_msg_obg.
        es_msg-msgv1  = 'Data Fim Contrato'.
        EXIT.

      ENDIF.

      IF is_row-material <> is_row_old-material.

        es_msg-msgno  = gc_msg_sm.
        es_msg-msgv1  = 'Material'.
        EXIT.

      ENDIF.

      IF is_row-lote <> is_row_old-lote.

        es_msg-msgno  = gc_msg_sm.
        es_msg-msgv1  = 'Lote'.
        EXIT.

      ENDIF.

      IF is_row-local_instalacao <> is_row_old-local_instalacao.

        es_msg-msgno  = gc_msg_sm.
        es_msg-msgv1  = 'Local de Instalação'.
        EXIT.

      ENDIF.

      IF is_row-horimetro_inicio <> is_row_old-horimetro_inicio.

        es_msg-msgno  = gc_msg_sm.
        es_msg-msgv1  = 'Horímetro Inicial'.
        EXIT.

      ENDIF.

      IF is_row-horimetro_fim IS INITIAL.

        es_msg-msgno  = gc_msg_obg.
        es_msg-msgv1  = 'Horímetro Final'.
        EXIT.

      ENDIF.

      IF is_row-centro_lucro <> is_row_old-centro_lucro.

        es_msg-msgno  = gc_msg_sm.
        es_msg-msgv1  = 'Centro de Lucro'.
        EXIT.

      ENDIF.

      IF is_row-ordem_devolucao IS NOT INITIAL.

        IF is_row-data_fim <> is_row_old-data_fim.

          es_msg-msgno  = gc_msg_dev.
          es_msg-msgv1  = 'Data Fim Contrato'.
          EXIT.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD update_cost_center.

    DATA(lt_rows) = go_salv->get_selections( )->get_selected_rows( ).

    IF lines( lt_rows ) = 1.

      DATA(ls_data) = VALUE #( gt_data[ lt_rows[ 1 ] ] OPTIONAL ).

      SELECT * UP TO 1 ROWS
         FROM cepc
         INTO @DATA(ls_cepc)
         WHERE prctr = @ls_data-centro_lucro.
       ENDSELECT.

       IF sy-subrc IS NOT INITIAL.

          MESSAGE s007(zmovativ) DISPLAY LIKE gc_erro WITH 'Centro de Lucro'.
          EXIT.

       ELSE.

         IF ls_cepc-prctr+0(4) <> ls_data-nome_centro+0(4).

            MESSAGE s006(zmovativ) DISPLAY LIKE gc_erro WITH 'Centro de Lucro'.
            EXIT.

         ELSE.

           DATA(lt_data) = VALUE gtt_fdata( FOR ls_row IN gt_data WHERE ( contrato = ls_data-contrato ) ( CORRESPONDING #( ls_row ) ) ).
           DELETE lt_data WHERE centro_lucro = ls_data-centro_lucro.

           LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lf_data>).

             <lf_data>-flg_contract = abap_true.
             <lf_data>-centro_lucro = ls_data-centro_lucro.

           ENDLOOP.

           me->call_bapi_cust_contract( lt_data ).


         ENDIF.

       ENDIF.

    ELSE.
      MESSAGE s007(zmovativ) DISPLAY LIKE gc_erro.
      EXIT.
    ENDIF.


  ENDMETHOD.

ENDCLASS.
