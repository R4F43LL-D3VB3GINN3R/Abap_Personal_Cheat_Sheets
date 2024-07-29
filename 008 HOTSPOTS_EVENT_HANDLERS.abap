*&---------------------------------------------------------------------*
*& Report ZALV_TEST002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zalv_test002.

"definicao para a classe de eventos
CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS:
*Hotspot click control
     handle_hotspot_click "metodo para inserir um hotspot
            FOR EVENT hotspot_click OF cl_gui_alv_grid
                IMPORTING e_row_id e_column_id es_row_no.
    METHODS: "metodo paras genreciar mudancas de dados
     handle_data_changed
            FOR EVENT data_changed OF cl_gui_alv_grid
                IMPORTING er_data_changed.
ENDCLASS.
"implementacao de metodos para classe de evento
CLASS lcl_event_handler IMPLEMENTATION .
  "eventos de clique
  METHOD handle_hotspot_click .
    PERFORM handle_hotspot_click USING e_row_id e_column_id es_row_no .
  ENDMETHOD.
  "alteracoes de dados
  METHOD handle_data_changed .
    PERFORM handle_data_changed USING er_data_changed .
  ENDMETHOD.
ENDCLASS.

*Definição Global para ALV
*Objeto do Tipo ALV Grid
DATA gr_alvgrid TYPE REF TO cl_gui_alv_grid.

*Nome do Custom Control adicionado à tela 100
CONSTANTS gc_custom_control_name TYPE scrfname VALUE 'CC_ALV'.

*Instância do Container
DATA gr_ccontainer TYPE REF TO cl_gui_custom_container.

*Tabela Interna do Catálogo de Campos
DATA gt_fieldcat TYPE lvc_t_fcat.

*Estrutura do Layout
DATA gs_layout TYPE lvc_s_layo.

"Tabela Interna.
DATA it_t001 TYPE TABLE OF t001.

"Parametro do metodo é uma tabela que recebe linhas referentes aos botoes do alv
DATA gt_exclude TYPE ui_functions.

START-OF-SELECTION.

  "Tela de Trabalho.
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*& Module SELECT_DATA OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE select_data OUTPUT.

  SELECT * FROM t001 INTO TABLE it_t001.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module DISPLAY_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE display_alv OUTPUT.

  PERFORM display_alv.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form display_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_alv .

  "instancia da classe de eventos
  DATA gr_event_handler TYPE REF TO lcl_event_handler.
  CREATE OBJECT gr_event_handler.

  "verifica se existe uma instância do alv
  IF gr_alvgrid IS INITIAL .
*----Creating custom container instance
    CREATE OBJECT gr_ccontainer
      EXPORTING
        container_name              = gc_custom_control_name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6 .
    IF sy-subrc <> 0.
*--Exception handling
    ENDIF.

*    *Instância do ALV Grid
      CREATE OBJECT gr_alvgrid
        EXPORTING
          i_parent          = gr_ccontainer
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5 .

      "prepara o catalogo de campos
      PERFORM prepare_field_catalog CHANGING gt_fieldcat .

      "prepara estrutura de layout
*      PERFORM prepare_layout CHANGING gs_layout .

      PERFORM exclude_tb_functions CHANGING gt_exclude. "insere as linhas na tabela de exclusao de funcionalidades

       SET HANDLER gr_event_handler->handle_hotspot_click FOR gr_alvgrid .

       SET HANDLER gr_event_handler->handle_data_changed  FOR gr_alvgrid.

       "funcao para controlar edicao de dados no alv
       CALL METHOD gr_alvgrid->register_edit_event
         EXPORTING
           i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*         EXCEPTIONS
*           error      = 1
*           others     = 2
               .
       IF sy-subrc <> 0.
*        Implement suitable error handling here
       ENDIF.


      "metodo para exibir os dados pela primeira vez
      CALL METHOD gr_alvgrid->set_table_for_first_display
        EXPORTING
          is_layout                     = gs_layout
          it_toolbar_excluding          = gt_exclude "tabela com os botoes das funcionalidades a serem excluidas
        CHANGING
          it_outtab                     = it_t001[]
          it_fieldcatalog               = gt_fieldcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4 .
    ELSE .
      "metodo para dar refresh no alv, caso a instancia já tenha sido criada
      CALL METHOD gr_alvgrid->refresh_table_display
        EXCEPTIONS
          finished       = 1
          OTHERS         = 2 .
    ENDIF .

ENDFORM .

FORM prepare_field_catalog CHANGING pt_fieldcat TYPE lvc_t_fcat .
DATA ls_fcat TYPE lvc_s_fcat .
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name       = 'T001'
       CHANGING
            ct_fieldcat            = pt_fieldcat[]
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.
  LOOP AT pt_fieldcat INTO ls_fcat.
      CASE ls_fcat-fieldname.
        WHEN 'BUKRS'.
          ls_fcat-hotspot = 'X'.
          MODIFY pt_fieldcat FROM ls_fcat.
        WHEN 'BUTXT'.
          ls_fcat-edit = 'X'.
          MODIFY pt_fieldcat FROM ls_fcat.
      ENDCASE.
  ENDLOOP.

ENDFORM.

FORM exclude_tb_functions CHANGING pt_exclude TYPE ui_functions .

  "inserimos na tabela o nome dos botoes que queremos excluir do alv
  "com eles as suas funcionalidades

  DATA ls_exclude TYPE ui_func.
  ls_exclude = cl_gui_alv_grid=>mc_fc_maximum .
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_minimum .
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_subtot .
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_sum .
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_average .
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_mb_sum .
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_mb_subtot .

ENDFORM .
*&---------------------------------------------------------------------*
*& Form handle_hotspot_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_ID
*&      --> E_COLUMN_ID
*&      --> ES_ROW_NO
*&---------------------------------------------------------------------*
FORM handle_hotspot_click  USING    p_e_row_id
                                    p_e_column_id
                                    p_es_row_no.

  "lendo as informacoes da linha do alv.
  READ TABLE it_t001 INTO DATA(wa_t001) INDEX p_e_row_id.

    MESSAGE wa_t001-butxt TYPE 'I'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form handle_data_changed
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ER_DATA_CHANGED
*&---------------------------------------------------------------------*

FORM handle_data_changed USING ir_data_changed
                         TYPE REF TO cl_alv_changed_data_protocol.

  DATA : ls_mod_cell  TYPE lvc_s_modi  ,
         lv_value      TYPE lvc_value  .

  SORT ir_data_changed->mt_mod_cells BY row_id.
  LOOP AT ir_data_changed->mt_mod_cells
                         INTO ls_mod_cell.

  ENDLOOP .

ENDFORM.                    " handle_data_changed
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
SET PF-STATUS 'ST'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'SAVE'.
      BREAK-POINT.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

* Estrutura do Field Catalog
DATA: it_fieldcat TYPE slis_t_fieldcat_alv,
      wa_fieldcat TYPE slis_fieldcat_alv.

CLEAR wa_fieldcat.
wa_fieldcat-col_pos = 1.
wa_fieldcat-fieldname = 'vbeln'.
wa_fieldcat-key = 'X'.
wa_fieldcat-hotspot = 'X'.
wa_fieldcat-seltext_m = 'Número do Documento'.
wa_fieldcat-just = 'C'.
wa_fieldcat-outputlen = 19.
APPEND wa_fieldcat TO it_fieldcat.

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    i_callback_program      = sy-repid
    i_callback_user_command = 'USER_COMMAND'
    i_grid_title            = 'Título da Grade ALV'
    it_fieldcat             = it_fieldcat
  TABLES
    t_outtab                = t_output.

FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN '&IC1'. " Este é o comando padrão para hotspots
      READ TABLE t_output INDEX rs_selfield-tabindex INTO ls_output.
      IF sy-subrc = 0.
        PERFORM call_transaction USING ls_output-vbeln.
      ENDIF.
  ENDCASE.

ENDFORM.

FORM call_transaction USING p_vbeln TYPE vbak-vbeln.

  DATA: lv_command TYPE string.

  " Monta o comando para a transação SE16N com o número do documento
  CONCATENATE '/nSE16N' 'VBAK' INTO lv_command SEPARATED BY space.

  " Adiciona o valor do número do documento ao comando
  SET PARAMETER ID 'VBL' FIELD p_vbeln.

  " Chama a transação SE16N com o comando montado
  CALL TRANSACTION 'SE16N' AND SKIP FIRST SCREEN.

ENDFORM.
