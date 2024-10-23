*----------------------------------------------------------------------*
***INCLUDE /SBXC/CONC_EXCEL_FICHACONC_F01.
*$---------------------------------------------------------------------*
*$*$ MODIFICAÇÕES                                                      *
*$---------------------------------------------------------------------*
*& Autor        : R. Marinheiro (SBX)
*& Data         : 2019-04-09
*& Referência   : SBX - Cockpit Conciliações V.1.0.18
*& Transporte   : DR1K901670
*& Ch. pesquisa : RSM-ID-18.01
*& Objetivo     : S/4HANA Readiness check
*&---------------------------------------------------------------------*
*& Autor        : R. Marinheiro (SBX)
*& Data         : 2019-04-09
*& Referência   : SBX - Cockpit Conciliações V.1.0.18
*& Transporte   : DR1K901670
*& Ch. pesquisa : RSM-ID-18.02
*& Objetivo     : Code cleanup (a remover na próxima versão)
*&---------------------------------------------------------------------*
*& Autor        : R. Marinheiro (SBX)
*& Data         : 2019-04-09
*& Referência   : SBX - Cockpit Conciliações V.1.0.18
*& Transporte   : DR1K901670
*& Ch. pesquisa : RSM-ID-18.05
*& Objetivo     : S/4HANA - Alteração no acesso à tabela T881
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F4_PATH
*&---------------------------------------------------------------------*
*       F4 para o caminho do ficheiro
*----------------------------------------------------------------------*
*      <--P_P_PATH  Caminho (ecrã de seleção)
*----------------------------------------------------------------------*
FORM f4_path  CHANGING p_path.
  DATA: ltp_filename TYPE string,
        ltp_path     TYPE string,
        ltp_fullpath TYPE string,
        ltp_initdir  TYPE string.

  ltp_initdir = p_path.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title              = 'Ficheiro de destino'
      default_extension         = 'xlsx'
      default_file_name         = '*.xlsx'
*     with_encoding             =
      file_filter               = 'Excel files (*.xlsx)|*.xlsx'
      initial_directory         = ltp_initdir
      prompt_on_overwrite       = 'X'
    CHANGING
      filename                  = ltp_filename
      path                      = ltp_path
      fullpath                  = ltp_fullpath
*     user_action               =
*     file_encoding             =
    EXCEPTIONS
      cntl_error                = 1
      error_no_gui              = 2
      not_supported_by_gui      = 3
      invalid_default_file_name = 4
      OTHERS                    = 5.

  p_path = ltp_fullpath.
ENDFORM.                    "f4_path

*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECTS
*&---------------------------------------------------------------------*
*       Criação de objetos
*----------------------------------------------------------------------*
FORM create_objects .
* Criar objeto para ficheiro excel
  CREATE OBJECT o_excel.
ENDFORM.                    "create_objects
*&---------------------------------------------------------------------*
*&      Form  CREATE_STYLES
*&---------------------------------------------------------------------*
*       Cria os estilos usados no ficheiro Excel
*----------------------------------------------------------------------*
FORM create_styles .
  " Create border object
  CREATE OBJECT o_border_dark.
  o_border_dark->border_color-rgb = zcl_excel_style_color=>c_black.
  o_border_dark->border_style = zcl_excel_style_border=>c_border_thin.
  CREATE OBJECT o_border_light.
  o_border_light->border_color-rgb = zcl_excel_style_color=>c_gray.
  o_border_light->border_style = zcl_excel_style_border=>c_border_thin.

* Bold, tam 8, centrado
  o_style_bold_center                   = o_excel->add_new_style( ).
  o_style_bold_center->font->bold       = abap_true.
  o_style_bold_center->font->italic     = abap_false.
  o_style_bold_center->font->name       = zcl_excel_style_font=>c_name_arial.
  o_style_bold_center->font->scheme     = zcl_excel_style_font=>c_scheme_none.
  o_style_bold_center->font->size       = 8.
  o_style_bold_center->font->color-rgb  = zcl_excel_style_color=>c_black.
  o_style_bold_center->alignment->horizontal  = zcl_excel_style_alignment=>c_horizontal_center.
  tp_style_bold_center_guid              = o_style_bold_center->get_guid( ).

* Bold, tam 8, left
  o_style_bold_left                   = o_excel->add_new_style( ).
  o_style_bold_left->font->bold       = abap_true.
  o_style_bold_left->font->italic     = abap_false.
  o_style_bold_left->font->name       = zcl_excel_style_font=>c_name_arial.
  o_style_bold_left->font->scheme     = zcl_excel_style_font=>c_scheme_none.
  o_style_bold_left->font->size       = 8.
  o_style_bold_left->font->color-rgb  = zcl_excel_style_color=>c_black.
  o_style_bold_left->alignment->horizontal  = zcl_excel_style_alignment=>c_horizontal_left.
  tp_style_bold_left_guid              = o_style_bold_left->get_guid( ).

* Normal, tam 8, left
  o_style_normal                   = o_excel->add_new_style( ).
  o_style_normal->font->bold       = abap_false.
  o_style_normal->font->italic     = abap_false.
  o_style_normal->font->name       = zcl_excel_style_font=>c_name_arial.
  o_style_normal->font->scheme     = zcl_excel_style_font=>c_scheme_none.
  o_style_normal->font->size       = 8.
  o_style_normal->font->color-rgb  = zcl_excel_style_color=>c_black.
  tp_style_normal_guid              = o_style_normal->get_guid( ).

* Bold, tam 8, centrado
  o_style_bold_center_border                   = o_excel->add_new_style( ).
  o_style_bold_center_border->font->bold       = abap_true.
  o_style_bold_center_border->font->italic     = abap_false.
  o_style_bold_center_border->font->name       = zcl_excel_style_font=>c_name_arial.
  o_style_bold_center_border->font->scheme     = zcl_excel_style_font=>c_scheme_none.
  o_style_bold_center_border->font->size       = 8.
  o_style_bold_center_border->font->color-rgb  = zcl_excel_style_color=>c_black.
  o_style_bold_center_border->alignment->horizontal  = zcl_excel_style_alignment=>c_horizontal_center.
  o_style_bold_center_border->borders->allborders    = o_border_dark.
  tp_style_bold_center_bord_guid              = o_style_bold_center_border->get_guid( ).

* Bold, tam 8, left
  o_style_bold_left_border                   = o_excel->add_new_style( ).
  o_style_bold_left_border->font->bold       = abap_true.
  o_style_bold_left_border->font->italic     = abap_false.
  o_style_bold_left_border->font->name       = zcl_excel_style_font=>c_name_arial.
  o_style_bold_left_border->font->scheme     = zcl_excel_style_font=>c_scheme_none.
  o_style_bold_left_border->font->size       = 8.
  o_style_bold_left_border->font->color-rgb  = zcl_excel_style_color=>c_black.
  o_style_bold_left_border->borders->allborders    = o_border_dark.
  tp_style_bold_left_border_guid              = o_style_bold_left_border->get_guid( ).

* Bold, tam 8, number
  o_style_bold_num_border                   = o_excel->add_new_style( ).
  o_style_bold_num_border->font->bold       = abap_true.
  o_style_bold_num_border->font->italic     = abap_false.
  o_style_bold_num_border->font->name       = zcl_excel_style_font=>c_name_arial.
  o_style_bold_num_border->font->scheme     = zcl_excel_style_font=>c_scheme_none.
  o_style_bold_num_border->font->size       = 8.
  o_style_bold_num_border->font->color-rgb  = zcl_excel_style_color=>c_black.
  o_style_bold_num_border->borders->allborders    = o_border_dark.
  o_style_bold_num_border->number_format->format_code    = zcl_excel_style_number_format=>c_format_number_00.
  tp_style_bold_num_border_guid              = o_style_bold_num_border->get_guid( ).

* Normal, tam 8, left
  o_style_normal_border                   = o_excel->add_new_style( ).
  o_style_normal_border->font->bold       = abap_false.
  o_style_normal_border->font->italic     = abap_false.
  o_style_normal_border->font->name       = zcl_excel_style_font=>c_name_arial.
  o_style_normal_border->font->scheme     = zcl_excel_style_font=>c_scheme_none.
  o_style_normal_border->font->size       = 8.
  o_style_normal_border->font->color-rgb  = zcl_excel_style_color=>c_black.
  o_style_normal_border->borders->allborders    = o_border_dark.
  tp_style_normal_border_guid              = o_style_normal_border->get_guid( ).

* Normal, tam 8, center
  o_style_normal_bord_cen                   = o_excel->add_new_style( ).
  o_style_normal_bord_cen->font->bold       = abap_false.
  o_style_normal_bord_cen->font->italic     = abap_false.
  o_style_normal_bord_cen->font->name       = zcl_excel_style_font=>c_name_arial.
  o_style_normal_bord_cen->font->scheme     = zcl_excel_style_font=>c_scheme_none.
  o_style_normal_bord_cen->font->size       = 8.
  o_style_normal_bord_cen->font->color-rgb  = zcl_excel_style_color=>c_black.
  o_style_normal_bord_cen->alignment->horizontal  = zcl_excel_style_alignment=>c_horizontal_center.
  o_style_normal_bord_cen->borders->allborders    = o_border_dark.
  tp_style_normal_bord_cen_guid              = o_style_normal_bord_cen->get_guid( ).

* Normal, tam 8, left, number
  o_style_number_border                   = o_excel->add_new_style( ).
  o_style_number_border->font->bold       = abap_false.
  o_style_number_border->font->italic     = abap_false.
  o_style_number_border->font->name       = zcl_excel_style_font=>c_name_arial.
  o_style_number_border->font->scheme     = zcl_excel_style_font=>c_scheme_none.
  o_style_number_border->font->size       = 8.
  o_style_number_border->font->color-rgb  = zcl_excel_style_color=>c_black.
  o_style_number_border->borders->allborders    = o_border_dark.
  o_style_number_border->number_format->format_code    = zcl_excel_style_number_format=>c_format_number_00.
  tp_style_number_border_guid              = o_style_number_border->get_guid( ).



* Gray 12,5% filling
  o_style_gray125                     = o_excel->add_new_style( ).
  o_style_gray125->fill->filltype     = zcl_excel_style_fill=>c_fill_pattern_gray125.
  o_style_gray125->borders->allborders    = o_border_dark.
  o_style_gray125->font->bold       = abap_true.
  o_style_gray125->font->italic     = abap_false.
  o_style_gray125->font->name       = zcl_excel_style_font=>c_name_arial.
  o_style_gray125->font->scheme     = zcl_excel_style_font=>c_scheme_none.
  o_style_gray125->font->size       = 8.
  o_style_gray125->font->color-rgb  = zcl_excel_style_color=>c_black.
  o_style_gray125->alignment->horizontal  = zcl_excel_style_alignment=>c_horizontal_center.
  tp_style_gray125_guid                = o_style_gray125->get_guid( ).

* Gray
  o_style_gray                     = o_excel->add_new_style( ).
  o_style_gray->font->bold       = abap_false.
  o_style_gray->font->italic     = abap_false.
  o_style_gray->font->name       = zcl_excel_style_font=>c_name_arial.
  o_style_gray->font->scheme     = zcl_excel_style_font=>c_scheme_none.
  o_style_gray->font->size       = 8.
  o_style_gray->font->color-rgb  = zcl_excel_style_color=>c_black.
  o_style_gray->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  o_style_gray->fill->fgcolor-rgb = zcl_excel_style_color=>c_gray.
  o_style_gray->borders->allborders    = o_border_dark.
  tp_style_gray_guid                = o_style_gray->get_guid( ).

* Gray, number
  o_style_gray_num                     = o_excel->add_new_style( ).
  o_style_gray_num->font->bold       = abap_false.
  o_style_gray_num->font->italic     = abap_false.
  o_style_gray_num->font->name       = zcl_excel_style_font=>c_name_arial.
  o_style_gray_num->font->scheme     = zcl_excel_style_font=>c_scheme_none.
  o_style_gray_num->font->size       = 8.
  o_style_gray_num->font->color-rgb  = zcl_excel_style_color=>c_black.
  o_style_gray_num->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  o_style_gray_num->fill->fgcolor-rgb = zcl_excel_style_color=>c_gray.
  o_style_gray_num->borders->allborders    = o_border_dark.
  o_style_gray_num->number_format->format_code    = zcl_excel_style_number_format=>c_format_number_00.
  tp_style_gray_num_guid                = o_style_gray_num->get_guid( ).
ENDFORM.                    "create_styles
*&---------------------------------------------------------------------*
*&      Form  SET_WORKSHEET_ATTRIBUTES
*&---------------------------------------------------------------------*
*       Atributos da folha de cálculo
*----------------------------------------------------------------------*
FORM set_worksheet_attributes .
  DATA: ltp_message TYPE string.
  TRY.
*     Obter folha de cálculo ativa
      o_worksheet = o_excel->get_active_worksheet( ).

*     Nome da folha
      o_worksheet->set_title( ip_title = 'IAS' ).
*     Não exibir gridlines
      o_worksheet->set_show_gridlines( abap_false ).
*     Margens
      o_worksheet->sheet_setup->set_page_margins( ip_top = '0.5' ip_bottom = '0' ip_left = '1'
                                 ip_right = '1' ip_header = '0' ip_footer = '0' ip_unit = 'cm' ).
*     Orientação - portrait
      o_worksheet->sheet_setup->orientation = zcl_excel_sheet_setup=>c_orientation_portrait.

*     Tamanho das colunas (aumentar 0.7 para assumir o tamanho desejado)
      o_worksheet->set_column_width( ip_column = 'A' ip_width_fix = '10.56' ).
      o_worksheet->set_column_width( ip_column = 'B' ip_width_fix = '9.13' ).
      o_worksheet->set_column_width( ip_column = 'C' ip_width_fix = '38.99' ).
      o_worksheet->set_column_width( ip_column = 'D' ip_width_fix = '10.41' ).
      o_worksheet->set_column_width( ip_column = 'E' ip_width_fix = '10.41' ).
      o_worksheet->set_column_width( ip_column = 'F' ip_width_fix = '10.41' ).
      o_worksheet->set_column_width( ip_column = 'G' ip_width_fix = '10.41' ).

*
*      DO 600 TIMES.
*        o_worksheet->set_row_height( ip_row = sy-index ip_height_fix = '12.75' ).
*      ENDDO.

    CATCH zcx_excel INTO x_excel.
      ltp_message = x_excel->get_text( ).
      MESSAGE ltp_message TYPE 'E'.
  ENDTRY.
ENDFORM.                    "set_worksheet_attributes
*&---------------------------------------------------------------------*
*&      Form  CREATE_WORKSHEET_CONTENT
*&---------------------------------------------------------------------*
*       Preencher a folha de cálculo
*----------------------------------------------------------------------*
FORM create_worksheet_content .
  TYPES: BEGIN OF lty_balance,
           header    TYPE char1,
           acc_s     TYPE racct,
           tsl_s     TYPE vtcur9,
           hsl_s     TYPE vtcur9,
           acc_f     TYPE racct,
           tsl_f     TYPE vtcur9,
           hsl_f     TYPE vtcur9,
           count_det TYPE i,
         END OF lty_balance.
  DATA: ltp_new_setid               TYPE sethier-setid,
        lit_set_values              TYPE TABLE OF rgsb4,
        lwa_set_values              TYPE rgsb4,
        lit_r_blart                 TYPE RANGE OF blart,
        lwa_r_blart                 LIKE LINE OF lit_r_blart,
        lit_balance                 TYPE TABLE OF lty_balance,
        lwa_balance                 TYPE lty_balance,
        ltp_tabix_config            TYPE sy-tabix,
        ltp_tabix_header            TYPE sy-tabix,
        ltp_tabix_item              TYPE sy-tabix,
        ltp_tabix_item_f            TYPE sy-tabix,
        ltp_flag_end_do_s           TYPE char1,
        ltp_flag_end_do_f           TYPE char1,
        ltp_new_group               TYPE char1,
        ltp_mitkz                   TYPE mitkz,
        ltp_cell_taxa_s             TYPE char10,
        ltp_cell_taxa_f             TYPE char10,
        ltp_formula_taxa            TYPE zexcel_cell_formula,
        ltp_formula_s1              TYPE zexcel_cell_formula,
        ltp_formula_s2              TYPE zexcel_cell_formula,
        ltp_formula_f1              TYPE zexcel_cell_formula,
        ltp_formula_f2              TYPE zexcel_cell_formula,
        ltp_formula_agrp_s1         TYPE zexcel_cell_formula,
        ltp_formula_agrp_s2         TYPE zexcel_cell_formula,
        ltp_formula_agrp_f1         TYPE zexcel_cell_formula,
        ltp_formula_agrp_f2         TYPE zexcel_cell_formula,
        ltp_formula_acc_sum_s1      TYPE zexcel_cell_formula,
        ltp_formula_acc_sum_s2      TYPE zexcel_cell_formula,
        ltp_formula_acc_sum_f1      TYPE zexcel_cell_formula,
        ltp_formula_acc_sum_f2      TYPE zexcel_cell_formula,
        ltp_formula_doc_sum_s2      TYPE zexcel_cell_formula,
        ltp_formula_doc_sum_f2      TYPE zexcel_cell_formula,
        ltp_formula_tot_view_sum_s2 TYPE zexcel_cell_formula,
        ltp_formula_tot_view_sum_f2 TYPE zexcel_cell_formula,
        ltp_formula_tot_file_sum_s2 TYPE zexcel_cell_formula,
        ltp_formula_tot_file_sum_f2 TYPE zexcel_cell_formula,
        lit_return                  TYPE TABLE OF bapiret2,
        lwa_address                 TYPE bapiaddr3,
        ltp_row_from                TYPE zexcel_cell_row,
        ltp_row_to                  TYPE zexcel_cell_row,
        ltp_acc_header_count        TYPE i,
        ltp_message                 TYPE string,
        ltp_value_string            TYPE string,
        lwa_mov_i_f                 TYPE ty_mov_i,
        lwa_mov_e_f                 TYPE ty_mov_e,
        ltp_ktopl                   TYPE ktopl, "RSM-ID-18.03
        ltp_index_f                 TYPE sy-tabix.

  TRY.
*     Linha 1
      tp_row = 1.
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = wa_conct058-text_01 ip_style = tp_style_bold_center_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'C' ip_value = '' ip_style = tp_style_bold_center_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = '' ip_style = tp_style_bold_center_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = '' ip_style = tp_style_bold_center_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_value = '' ip_style = tp_style_bold_center_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = '' ip_style = tp_style_bold_center_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = '' ip_style = tp_style_bold_center_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_value = '' ip_style = tp_style_bold_center_guid  ).
      o_worksheet->set_merge( ip_row = tp_row ip_column_start = 'A' ip_column_end = 'I' ).

*     Linha 3
      tp_row = 3.
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = wa_conct058-text_02 ip_style = tp_style_bold_left_guid  ).

*     Linha 4
      tp_row = 4.
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = 'Taxa' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = tp_exchange_rate_is ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = 'Taxa' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = tp_exchange_rate_if ip_style = tp_style_bold_center_bord_guid  ).
      ltp_cell_taxa_s = '$E$4'.
      ltp_cell_taxa_f = '$H$4'.

*     Linha 5
      tp_row = 5.
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = wa_conct058-text_03 ip_style = tp_style_normal_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = 'N/CONTAS EM ANÁLISE' ip_style = tp_style_gray125_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = '' ip_style = tp_style_gray125_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_value = '' ip_style = tp_style_gray125_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = '' ip_style = tp_style_gray125_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = '' ip_style = tp_style_gray125_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_value = '' ip_style = tp_style_gray125_guid  ).
      o_worksheet->set_merge( ip_row = tp_row ip_column_start = 'D' ip_column_end = 'I' ).

*     Linha 6
      tp_row = 6.
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = 'SALDOS' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_merge( ip_row = tp_row ip_column_start = 'D' ip_column_end = 'F' ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = 'FLUXOS' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_merge( ip_row = tp_row ip_column_start = 'G' ip_column_end = 'I' ).

*     Linha 7
      tp_row = 7.
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = wa_conct058-text_04 ip_style = tp_style_normal_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = 'Conta SNC' ip_style = tp_style_bold_center_bord_guid  ).
      CONCATENATE 'Valor ' wa_t001-waers INTO ltp_value_string RESPECTING BLANKS.
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = ltp_value_string ip_style = tp_style_bold_center_bord_guid  ).
      CONCATENATE 'Valor ' wa_conct058-moeda_reporte INTO ltp_value_string RESPECTING BLANKS.
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_value = ltp_value_string ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = 'Conta SNC' ip_style = tp_style_bold_center_bord_guid  ).
      CONCATENATE 'Valor ' wa_t001-waers INTO ltp_value_string RESPECTING BLANKS.
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = ltp_value_string ip_style = tp_style_bold_center_bord_guid  ).
      CONCATENATE 'Valor ' wa_conct058-moeda_reporte INTO ltp_value_string RESPECTING BLANKS.
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_value = ltp_value_string ip_style = tp_style_bold_center_bord_guid  ).

**********************************************************************
*     Totais por conta
**********************************************************************
      ltp_tabix_config = 1.
      CLEAR: ltp_flag_end_do_s, ltp_flag_end_do_f.

      DO.
*       Saldos
        READ TABLE it_conct055_s INTO wa_conct055_s INDEX ltp_tabix_config.
        IF sy-subrc EQ 0.
*         Insere linha de cabeçalho
          CLEAR lwa_balance.
          lwa_balance-header  = abap_true.
          lwa_balance-acc_s   = wa_conct055_s-agrupador.
          APPEND lwa_balance TO lit_balance.
          ltp_tabix_header = sy-tabix. "guarda o indice da linha de header

          ltp_tabix_item = ltp_tabix_header.
*         Pesquisa as contas que respeitam a regra do agrupador
*          LOOP AT t_saldos_conta INTO wa_saldos_conta.
          LOOP AT it_saldos_i INTO wa_saldos_i.
            IF wa_saldos_i-racct CP wa_conct055_s-pesquisa.
*             Se tiver o tipo de conta definido, verifica se a conta pertence ao argupador
              IF NOT wa_conct055_s-mitkz IS INITIAL.
*               RSM-ID-18.03-ini
*                SELECT SINGLE mitkz INTO ltp_mitkz FROM skb1 WHERE bukrs = p_bukrs AND saknr = wa_saldos_i_f-racct.

                SELECT SINGLE ktopl FROM t001 INTO ltp_ktopl WHERE bukrs = p_bukrs.
                SELECT              "#EC CI_DB_OPERATION_OK[0002431747]
                SINGLE mitkz FROM skb1 AS b
                INNER JOIN          "#EC CI_DB_OPERATION_OK[0002389136]
                 ska1 AS a ON a~saknr = b~saknr
                INTO ltp_mitkz
                WHERE b~bukrs = p_bukrs
                AND b~saknr = wa_saldos_i_f-racct
                AND a~ktopl = ltp_ktopl
                AND a~glaccount_type NE 'S'             "#EC CI_NOORDER
.
*               RSM-ID-18.03-fim

                IF sy-subrc EQ 0 AND ltp_mitkz NE wa_conct055_s-mitkz.
                  CONTINUE.
                ENDIF.
              ENDIF.

              CLEAR lwa_balance.
              ADD 1 TO ltp_tabix_item.
              lwa_balance-acc_s = wa_saldos_i-racct.
              lwa_balance-tsl_s = wa_saldos_i-tsl.
              lwa_balance-hsl_s = wa_saldos_i-hsl.
              INSERT lwa_balance INTO lit_balance INDEX ltp_tabix_item.
              DELETE it_saldos_i.
            ENDIF.
          ENDLOOP.
          IF sy-subrc EQ 0 AND ltp_tabix_item GT ltp_tabix_header.
            READ TABLE lit_balance INTO lwa_balance INDEX ltp_tabix_header.

            lwa_balance-count_det   = ltp_tabix_item - ltp_tabix_header.
            MODIFY lit_balance FROM lwa_balance INDEX ltp_tabix_header.
          ENDIF.
        ELSE.
          ltp_flag_end_do_s = abap_true.
        ENDIF.

*       Fluxos
        READ TABLE it_conct055_f INTO wa_conct055_f INDEX ltp_tabix_config.
        IF sy-subrc EQ 0.
*         Altera linha de cabeçalho
          READ TABLE lit_balance INTO lwa_balance INDEX ltp_tabix_header.
          lwa_balance-acc_f   = wa_conct055_f-agrupador.
          MODIFY lit_balance FROM lwa_balance INDEX ltp_tabix_header.

          ltp_tabix_item_f = ltp_tabix_header.
*         Pesquisa as contas que respeitam a regra do argupador
          LOOP AT it_saldos_i_f INTO wa_saldos_i_f.
            IF wa_saldos_i_f-racct CP wa_conct055_f-pesquisa.
*             Se tiver o tipo de conta definido, verifica se a conta pertence ao agrupador
              IF NOT wa_conct055_f-mitkz IS INITIAL.
*               RSM-ID-18.03-ini
*                SELECT SINGLE mitkz INTO ltp_mitkz FROM skb1 WHERE bukrs = p_bukrs AND saknr = wa_saldos_i_f-racct.

                SELECT SINGLE ktopl FROM t001 INTO ltp_ktopl WHERE bukrs = p_bukrs.
                SELECT              "#EC CI_DB_OPERATION_OK[0002431747]
                SINGLE mitkz FROM skb1 AS b
                INNER JOIN          "#EC CI_DB_OPERATION_OK[0002389136]
                 ska1 AS a ON a~saknr = b~saknr
                INTO ltp_mitkz
                WHERE b~bukrs = p_bukrs
                AND b~saknr = wa_saldos_i_f-racct
                AND a~ktopl = ltp_ktopl
                AND a~glaccount_type NE 'S'             "#EC CI_NOORDER
.
*               RSM-ID-18.03-fim
                IF sy-subrc EQ 0 AND ltp_mitkz NE wa_conct055_f-mitkz.
                  CONTINUE.
                ENDIF.
              ENDIF.

              CLEAR lwa_balance.
              ADD 1 TO ltp_tabix_item_f.

              IF ltp_tabix_item_f LE ltp_tabix_item. "Saldos já tem conta preenchida, modifica a linha
                READ TABLE lit_balance INTO lwa_balance  INDEX ltp_tabix_item_f.
                lwa_balance-acc_f = wa_saldos_i_f-racct.
                lwa_balance-tsl_f = wa_saldos_i_f-tsl.
                lwa_balance-hsl_f = wa_saldos_i_f-hsl.
                MODIFY lit_balance FROM lwa_balance INDEX ltp_tabix_item_f.
              ELSE. "Fluxos tem mais contas do que saldos, acrescenta as linhas extra
                lwa_balance-acc_f = wa_saldos_i_f-racct.
                lwa_balance-tsl_f = wa_saldos_i_f-tsl.
                lwa_balance-hsl_f = wa_saldos_i_f-hsl.
                INSERT lwa_balance INTO lit_balance INDEX ltp_tabix_item_f.
              ENDIF.
              DELETE it_saldos_i_f.
            ENDIF.
          ENDLOOP.
          IF sy-subrc EQ 0 AND ltp_tabix_item_f GT ltp_tabix_header.
            READ TABLE lit_balance INTO lwa_balance INDEX ltp_tabix_header.
            IF lwa_balance-count_det LT ( ltp_tabix_item_f - ltp_tabix_header ).
              lwa_balance-count_det = ltp_tabix_item_f - ltp_tabix_header .
            ENDIF.
            MODIFY lit_balance FROM lwa_balance INDEX ltp_tabix_header.
          ENDIF.
        ELSE.
          ltp_flag_end_do_f = abap_true.
        ENDIF.

*       Se não encontrar mais registos de saldos e fluxos, sai fora
        IF ltp_flag_end_do_s EQ abap_true AND ltp_flag_end_do_f EQ abap_true.
          EXIT.
        ENDIF.

        ADD 1 TO ltp_tabix_config.
      ENDDO.

**********************************************************************
*       Se ainda tiver contas de saldos que não foram distribuidas pelos agrupadores, coloca em "Outras"
*       Insere linha de cabeçalho
      CLEAR lwa_balance.
      lwa_balance-header  = abap_true.
      lwa_balance-acc_s   = 'Outras'.
      lwa_balance-acc_f   = 'Outras'.
      APPEND lwa_balance TO lit_balance.
      ltp_tabix_header = sy-tabix. "guarda o indice da linha de header

      ltp_tabix_item = ltp_tabix_header.
*       Contas restantes
      LOOP AT it_saldos_i INTO wa_saldos_i.
        CLEAR lwa_balance.
        ADD 1 TO ltp_tabix_item.
        lwa_balance-acc_s = wa_saldos_i-racct.
        lwa_balance-tsl_s = wa_saldos_i-tsl.
        lwa_balance-hsl_s = wa_saldos_i-hsl.
        INSERT lwa_balance INTO lit_balance INDEX ltp_tabix_item.
        DELETE it_saldos_i.
      ENDLOOP.
      IF sy-subrc EQ 0 AND ltp_tabix_item GT ltp_tabix_header.
        READ TABLE lit_balance INTO lwa_balance INDEX ltp_tabix_header.
        lwa_balance-count_det   = ltp_tabix_item - ltp_tabix_header.
        MODIFY lit_balance FROM lwa_balance INDEX ltp_tabix_header.
      ENDIF.
**********************************************************************
*       Se ainda tiver contas de saldos que não foram distribuidas pelos agrupadores, coloca em "Outras"
*       Insere linha de cabeçalho
      ltp_tabix_item_f = ltp_tabix_header.
*       Contas restantes
      LOOP AT it_saldos_i_f INTO wa_saldos_i_f.
        CLEAR lwa_balance.
        ADD 1 TO ltp_tabix_item_f.
        IF ltp_tabix_item_f LE ltp_tabix_item. "Saldos já tem conta preenchida, modifica a linha
          READ TABLE lit_balance INTO lwa_balance  INDEX ltp_tabix_item_f.
          lwa_balance-acc_f = wa_saldos_i_f-racct.
          lwa_balance-tsl_f = wa_saldos_i_f-tsl.
          lwa_balance-hsl_f = wa_saldos_i_f-hsl.
          MODIFY lit_balance FROM lwa_balance INDEX ltp_tabix_item_f.
        ELSE. "Fluxos tem mais contas do que saldos, acrescenta as linhas extra
          lwa_balance-acc_f = wa_saldos_i_f-racct.
          lwa_balance-tsl_f = wa_saldos_i_f-tsl.
          lwa_balance-hsl_f = wa_saldos_i_f-hsl.
          INSERT lwa_balance INTO lit_balance INDEX ltp_tabix_item_f.
        ENDIF.
        DELETE it_saldos_i_f.
      ENDLOOP.
      IF sy-subrc EQ 0 AND ltp_tabix_item_f GT ltp_tabix_header.
        READ TABLE lit_balance INTO lwa_balance INDEX ltp_tabix_header.
        IF lwa_balance-count_det LT ( ltp_tabix_item_f - ltp_tabix_header ).
          lwa_balance-count_det = ltp_tabix_item_f - ltp_tabix_header .
          MODIFY lit_balance FROM lwa_balance INDEX ltp_tabix_header.
        ENDIF.
      ENDIF.
**********************************************************************


*     Preencher excel com base na tabela auxiliar
      tp_row = 8.

      ltp_acc_header_count = 0.
      LOOP AT lit_balance INTO lwa_balance.
        CLEAR: ltp_row_from, ltp_row_to, ltp_formula_s1, ltp_formula_s2, ltp_formula_f1, ltp_formula_f2.

        IF lwa_balance-header EQ abap_true.
          ADD 1 TO ltp_acc_header_count.
          IF lwa_balance-count_det GT 0.
            ltp_row_from = tp_row + 1.
            ltp_row_to = tp_row + lwa_balance-count_det.
          ENDIF.

          IF ltp_acc_header_count EQ 2.
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = wa_conct058-text_05 ip_style = tp_style_normal_guid  ).
          ENDIF.

          IF ltp_acc_header_count EQ 5.
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = 'EMPRESA EMISSORA:   ' ip_style = tp_style_bold_left_guid   ).
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'C' ip_value = wa_t001-butxt ip_style = tp_style_bold_center_guid   ).
          ENDIF.

          IF ltp_acc_header_count EQ 6.
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'C' ip_value = wa_t001-rcomp ip_style = tp_style_bold_center_guid   ).
          ENDIF.

          IF ltp_acc_header_count EQ 10.
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = 'Data de referência: ' ip_style = tp_style_bold_left_guid   ).
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'C' ip_value = tp_budatf ip_style = tp_style_bold_left_guid   ).
          ENDIF.

          IF ltp_acc_header_count EQ 12.
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = 'O Responsável: ' ip_style = tp_style_normal_guid  ).
            CALL FUNCTION 'BAPI_USER_GET_DETAIL'
              EXPORTING
                username = sy-uname
              IMPORTING
                address  = lwa_address
              TABLES
                return   = lit_return.

            o_worksheet->set_cell( ip_row = tp_row ip_column = 'C' ip_value = lwa_address-fullname ip_style = tp_style_normal_guid  ).
          ENDIF.



*         Determinar as fórmulas de somatório
          IF lwa_balance-count_det GT 0.
            ltp_formula_s1 = 'SUM(E' && ltp_row_from && ':E' && ltp_row_to && ')'.
            ltp_formula_s2 = 'SUM(F' && ltp_row_from && ':F' && ltp_row_to && ')'.
            ltp_formula_f1 = 'SUM(H' && ltp_row_from && ':H' && ltp_row_to && ')'.
            ltp_formula_f2 = 'SUM(I' && ltp_row_from && ':I' && ltp_row_to && ')'.
          ENDIF.
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value   = lwa_balance-acc_s ip_style  = tp_style_normal_bord_cen_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_formula = ltp_formula_s1 ip_style     = tp_style_number_border_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_formula = ltp_formula_s2 ip_style     = tp_style_number_border_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value   = lwa_balance-acc_f ip_style  = tp_style_normal_bord_cen_guid ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_formula = ltp_formula_f1 ip_style     = tp_style_number_border_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_formula = ltp_formula_f2 ip_style     = tp_style_number_border_guid  ).

*         Somar ao totalizador de contas
          ltp_formula_acc_sum_s1 = ltp_formula_acc_sum_s1 && '+' && 'E' && tp_row.
          ltp_formula_acc_sum_s2 = ltp_formula_acc_sum_s2 && '+' && 'F' && tp_row.
          ltp_formula_acc_sum_f1 = ltp_formula_acc_sum_f1 && '+' && 'H' && tp_row.
          ltp_formula_acc_sum_f2 = ltp_formula_acc_sum_f2 && '+' && 'I' && tp_row.


*         Agrupar linhas de detalhe
          IF lwa_balance-count_det GT 0.
            o_worksheet->set_row_outline( iv_row_from = ltp_row_from iv_row_to = ltp_row_to iv_collapsed = abap_true ).
          ENDIF.
        ELSE.
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = lwa_balance-acc_s ip_style = tp_style_gray_num_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = lwa_balance-hsl_s ip_style = tp_style_gray_num_guid  ).
          ltp_formula_taxa = 'E' && tp_row && '*' && ltp_cell_taxa_s.
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_formula = ltp_formula_taxa ip_style = tp_style_gray_num_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = lwa_balance-acc_f ip_style = tp_style_gray_num_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = lwa_balance-hsl_f ip_style = tp_style_gray_num_guid  ).
          ltp_formula_taxa = 'H' && tp_row && '*' && ltp_cell_taxa_f.
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_formula = ltp_formula_taxa ip_style = tp_style_gray_num_guid  ).
        ENDIF.
        ADD 1 TO tp_row.
      ENDLOOP.

*     Totalizador das contas
      IF NOT ltp_formula_acc_sum_s1 IS INITIAL.
        IF ltp_formula_acc_sum_s1(1) EQ '+'.
          SHIFT:  ltp_formula_acc_sum_s1 LEFT BY 1 PLACES,
                  ltp_formula_acc_sum_s2 LEFT BY 1 PLACES,
                  ltp_formula_acc_sum_f1 LEFT BY 1 PLACES,
                  ltp_formula_acc_sum_f2 LEFT BY 1 PLACES.
        ENDIF.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_formula = ltp_formula_acc_sum_s1 ip_style = tp_style_bold_num_border_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_formula = ltp_formula_acc_sum_s2 ip_style = tp_style_bold_num_border_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_formula = ltp_formula_acc_sum_f1 ip_style = tp_style_bold_num_border_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_formula = ltp_formula_acc_sum_f2 ip_style = tp_style_bold_num_border_guid  ).

*       Totalizador da visão
        ltp_formula_tot_view_sum_s2 = ltp_formula_tot_view_sum_s2 && '+' && 'F' && tp_row.
        ltp_formula_tot_view_sum_f2 = ltp_formula_tot_view_sum_f2 && '+' && 'I' && tp_row.
      ENDIF.

*********************************************************************
*     Documentos
**********************************************************************

      ADD 2 TO tp_row.
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_merge( ip_row = tp_row ip_column_start = 'D' ip_column_end = 'F' ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_merge( ip_row = tp_row ip_column_start = 'G' ip_column_end = 'I' ).


      ADD 1 TO tp_row.
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = 'Doc. Nr.' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'B' ip_value = 'Data' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'C' ip_value = 'Movimentos em trânsito' ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = 'Conta SNC' ip_style = tp_style_bold_center_bord_guid  ).

      CONCATENATE 'Valor ' wa_t001-waers INTO ltp_value_string RESPECTING BLANKS.
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = ltp_value_string ip_style = tp_style_bold_center_bord_guid  ).
      CONCATENATE 'Valor ' wa_conct058-moeda_reporte INTO ltp_value_string RESPECTING BLANKS.
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_value = ltp_value_string ip_style = tp_style_bold_center_bord_guid  ).
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = 'Conta SNC' ip_style = tp_style_bold_center_bord_guid  ).
      CONCATENATE 'Valor ' wa_t001-waers INTO ltp_value_string RESPECTING BLANKS.
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = ltp_value_string ip_style = tp_style_bold_center_bord_guid  ).
      CONCATENATE 'Valor ' wa_conct058-moeda_reporte INTO ltp_value_string RESPECTING BLANKS.
      o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_value = ltp_value_string ip_style = tp_style_bold_center_bord_guid  ).

      LOOP AT it_conct056 INTO wa_conct056.
        CLEAR: ltp_new_setid, lit_set_values[], lwa_set_values, lwa_r_blart, lit_r_blart[].
        CLEAR: ltp_formula_agrp_s1, ltp_formula_agrp_s2, ltp_formula_agrp_f1, ltp_formula_agrp_f2.

*       Cabeçalho
        ADD 1 TO tp_row.
        ltp_row_from = tp_row.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = wa_conct056-text_m ip_style = tp_style_normal_border_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'B' ip_value = '' ip_style = tp_style_normal_border_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'C' ip_value = wa_conct056-text_l ip_style = tp_style_normal_border_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = '' ip_style = tp_style_normal_border_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = '' ip_style = tp_style_normal_border_guid  ).


*       Ler nome do set
        CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
          EXPORTING
            shortname                = wa_conct056-setname
          IMPORTING
            new_setid                = ltp_new_setid
          EXCEPTIONS
            no_set_found             = 1
            no_set_picked_from_popup = 2
            wrong_class              = 3
            wrong_subclass           = 4
            table_field_not_found    = 5
            fields_dont_match        = 6
            set_is_empty             = 7
            formula_in_set           = 8
            set_is_dynamic           = 9
            OTHERS                   = 10.
        IF sy-subrc EQ 0.

*         Carregar valores do set
          CALL FUNCTION 'G_SET_GET_ALL_VALUES'
            EXPORTING
              setnr         = ltp_new_setid
            TABLES
              set_values    = lit_set_values
            EXCEPTIONS
              set_not_found = 1
              OTHERS        = 2.
          IF sy-subrc EQ 0.

            LOOP AT lit_set_values INTO lwa_set_values.
              IF wa_conct056-exclude EQ 'X'.
                lwa_r_blart-sign = 'E'.
              ELSE.
                lwa_r_blart-sign = 'I'.
              ENDIF.
              IF lwa_set_values-to IS INITIAL OR lwa_set_values-to EQ lwa_set_values-from.
                lwa_r_blart-option = 'EQ'.
              ELSE.
                lwa_r_blart-option = 'BT'.
              ENDIF.
              lwa_r_blart-low = lwa_set_values-from.
              lwa_r_blart-high = lwa_set_values-to.
              APPEND lwa_r_blart TO lit_r_blart.
            ENDLOOP.
          ENDIF.
        ENDIF.

*       Percorrer movimentos com TD no set
        IF NOT lit_r_blart IS INITIAL.
*         Dividir por agrupadores de contas
          LOOP AT it_conct055_s INTO wa_conct055_s.
            ltp_tabix_header = tp_row.
            ltp_new_group = 'X'.
            LOOP AT it_mov_i INTO wa_mov_i WHERE sbxc_blart_01 IN lit_r_blart.

              IF wa_mov_i-racct CP wa_conct055_s-pesquisa.
*             Se tiver o tipo de conta definido, verifica se a conta pertence ao argupador
                IF NOT wa_conct055_s-mitkz IS INITIAL.
*               RSM-ID-18.03-ini
*                SELECT SINGLE mitkz INTO ltp_mitkz FROM skb1 WHERE bukrs = p_bukrs AND saknr = wa_saldos_i-racct.

                  SELECT SINGLE ktopl FROM t001 INTO ltp_ktopl WHERE bukrs = p_bukrs.
                  SELECT            "#EC CI_DB_OPERATION_OK[0002431747]
                  SINGLE mitkz FROM skb1 AS b
                  INNER JOIN        "#EC CI_DB_OPERATION_OK[0002389136]
                   ska1 AS a ON a~saknr = b~saknr
                  INTO ltp_mitkz
                  WHERE b~bukrs = p_bukrs
                  AND b~saknr = wa_saldos_i-racct
                  AND a~ktopl = ltp_ktopl
                  AND a~glaccount_type NE 'S'           "#EC CI_NOORDER
.
*               RSM-ID-18.03-fim

                  IF sy-subrc EQ 0 AND ltp_mitkz NE wa_conct055_s-mitkz.
                    CONTINUE.
                  ENDIF.
                ENDIF.
                IF ltp_new_group EQ 'X'.
                  ADD 1 TO tp_row. "Reserva linha para o cabeçalho do agrupador
                  ltp_tabix_header = tp_row.
                  CLEAR ltp_new_group.
                ENDIF.
*               Escreve documento
                ADD 1 TO tp_row.
                o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = wa_mov_i-docnr ip_style = tp_style_gray_guid  ).
                o_worksheet->set_cell( ip_row = tp_row ip_column = 'B' ip_value = wa_mov_i-budat ip_style = tp_style_gray_guid  ).
                o_worksheet->set_cell( ip_row = tp_row ip_column = 'C' ip_value = wa_mov_i-description ip_style = tp_style_gray_guid  ).
                o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = wa_mov_i-racct ip_style = tp_style_gray_guid  ).
                o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = wa_mov_i-hsl ip_style = tp_style_gray_num_guid   ).
                ltp_formula_taxa = 'E' && tp_row && '*' && ltp_cell_taxa_s.
                o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_formula = ltp_formula_taxa ip_style = tp_style_gray_num_guid  ).
*               Ler fluxos
                READ TABLE it_mov_i_f INTO lwa_mov_i_f WITH KEY docnr = wa_mov_i-docnr.
                ltp_index_f = sy-tabix.
                IF sy-subrc EQ 0.
                  o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = lwa_mov_i_f-racct ip_style = tp_style_gray_guid  ).
                  o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = lwa_mov_i_f-hsl ip_style = tp_style_gray_num_guid   ).
                  ltp_formula_taxa = 'H' && tp_row && '*' && ltp_cell_taxa_f.
                  o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_formula = ltp_formula_taxa ip_style = tp_style_gray_num_guid  ).
                  DELETE it_mov_i_f INDEX ltp_index_f. "Apaga para não listar de novo referente a outra conta de saldos
                ELSE.
                  o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = '' ip_style = tp_style_gray_guid  ).
                  o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = '' ip_style = tp_style_gray_num_guid   ).
                  o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_value = '' ip_style = tp_style_gray_num_guid ).
                ENDIF.

                DELETE it_mov_i. "Apaga para no final ter apenas as contas sem agrupador.
              ENDIF.
            ENDLOOP.

*           Se o agrupador tem movimentos, escreve o cabeçalho do agrupador
            IF ltp_tabix_header LT tp_row.
*             Determinar as fórmulas de somatório
              ADD 1 TO ltp_tabix_header.
              ltp_formula_s1 = 'SUM(E' && ltp_tabix_header && ':E' && tp_row && ')'.
              ltp_formula_s2 = 'SUM(F' && ltp_tabix_header && ':F' && tp_row && ')'.
              ltp_formula_f1 = 'SUM(H' && ltp_tabix_header && ':H' && tp_row && ')'.
              ltp_formula_f2 = 'SUM(I' && ltp_tabix_header && ':I' && tp_row && ')'.
              SUBTRACT 1 FROM ltp_tabix_header.
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'A' ip_value = '' ip_style = tp_style_normal_bord_cen_guid    ).
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'B' ip_value = '' ip_style = tp_style_normal_bord_cen_guid    ).
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'C' ip_value = '' ip_style = tp_style_normal_bord_cen_guid    ).
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'D' ip_value   = wa_conct055_s-agrupador ip_style  = tp_style_normal_bord_cen_guid  ).
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'E' ip_formula = ltp_formula_s1 ip_style     = tp_style_number_border_guid  ).
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'F' ip_formula = ltp_formula_s2 ip_style     = tp_style_number_border_guid  ).
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'G' ip_value   = '' ip_style  = tp_style_normal_bord_cen_guid  ).
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'H' ip_formula = ltp_formula_f1 ip_style     = tp_style_number_border_guid  ).
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'I' ip_formula = ltp_formula_f2 ip_style     = tp_style_number_border_guid  ).

*             Soma o agrupador na formula para o cabeçalho do grupo de movimentos
              ltp_formula_agrp_s1 = ltp_formula_agrp_s1 && '+' && 'E' && ltp_tabix_header.
              ltp_formula_agrp_s2 = ltp_formula_agrp_s2 && '+' && 'F' && ltp_tabix_header.
              ltp_formula_agrp_f1 = ltp_formula_agrp_f1 && '+' && 'H' && ltp_tabix_header.
              ltp_formula_agrp_f2 = ltp_formula_agrp_f2 && '+' && 'I' && ltp_tabix_header.

*             Agrupar linhas de detalhe
              ADD 1 TO ltp_tabix_header.
              o_worksheet->set_row_outline( iv_row_from = ltp_tabix_header iv_row_to = tp_row iv_collapsed = abap_true ).


            ENDIF.

          ENDLOOP.
**********************************************************************
*           Listar contas sem agrupador
          ltp_tabix_header = tp_row.
          ltp_new_group = 'X'.
          LOOP AT it_mov_i INTO wa_mov_i WHERE sbxc_blart_01 IN lit_r_blart.
            IF ltp_new_group EQ 'X'.
              ADD 1 TO tp_row. "Reserva linha para o cabeçalho do agrupador
              ltp_tabix_header = tp_row.
              CLEAR ltp_new_group.
            ENDIF.
*               Escreve documento
            ADD 1 TO tp_row.
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = wa_mov_i-docnr ip_style = tp_style_gray_guid  ).
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'B' ip_value = wa_mov_i-budat ip_style = tp_style_gray_guid  ).
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'C' ip_value = wa_mov_i-description ip_style = tp_style_gray_guid  ).
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = wa_mov_i-racct ip_style = tp_style_gray_guid  ).
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = wa_mov_i-hsl ip_style = tp_style_gray_num_guid   ).
            ltp_formula_taxa = 'E' && tp_row && '*' && ltp_cell_taxa_s.
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_formula = ltp_formula_taxa ip_style = tp_style_gray_num_guid  ).
*               Ler fluxos
            READ TABLE it_mov_i_f INTO lwa_mov_i_f WITH KEY docnr = wa_mov_i-docnr.
            ltp_index_f = sy-tabix.
            IF sy-subrc EQ 0.
              o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = lwa_mov_i_f-racct ip_style = tp_style_gray_guid  ).
              o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = lwa_mov_i_f-hsl ip_style = tp_style_gray_num_guid   ).
              ltp_formula_taxa = 'H' && tp_row && '*' && ltp_cell_taxa_f.
              o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_formula = ltp_formula_taxa ip_style = tp_style_gray_num_guid  ).
              DELETE it_mov_i_f INDEX ltp_index_f. "Apaga para não listar de novo referente a outra conta de saldos
            ELSE.
              o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = '' ip_style = tp_style_gray_guid  ).
              o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = '' ip_style = tp_style_gray_num_guid   ).
              o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_value = '' ip_style = tp_style_gray_num_guid ).
            ENDIF.

            DELETE it_mov_i. "Apaga para no final ter apenas as contas sem agrupador.
          ENDLOOP.
*           Se o agrupador tem movimentos, escreve o cabeçalho do agrupador
          IF ltp_tabix_header LT tp_row.
*             Determinar as fórmulas de somatório
            ADD 1 TO ltp_tabix_header.
            ltp_formula_s1 = 'SUM(E' && ltp_tabix_header && ':E' && tp_row && ')'.
            ltp_formula_s2 = 'SUM(F' && ltp_tabix_header && ':F' && tp_row && ')'.
            ltp_formula_f1 = 'SUM(H' && ltp_tabix_header && ':H' && tp_row && ')'.
            ltp_formula_f2 = 'SUM(I' && ltp_tabix_header && ':I' && tp_row && ')'.
            SUBTRACT 1 FROM ltp_tabix_header.
            o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'A' ip_value = '' ip_style = tp_style_normal_bord_cen_guid    ).
            o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'B' ip_value = '' ip_style = tp_style_normal_bord_cen_guid    ).
            o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'C' ip_value = '' ip_style = tp_style_normal_bord_cen_guid    ).
            o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'D' ip_value   = 'Outras' ip_style  = tp_style_normal_bord_cen_guid  ).
            o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'E' ip_formula = ltp_formula_s1 ip_style     = tp_style_number_border_guid  ).
            o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'F' ip_formula = ltp_formula_s2 ip_style     = tp_style_number_border_guid  ).
            o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'G' ip_value   = '' ip_style  = tp_style_normal_bord_cen_guid  ).
            o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'H' ip_formula = ltp_formula_f1 ip_style     = tp_style_number_border_guid  ).
            o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'I' ip_formula = ltp_formula_f2 ip_style     = tp_style_number_border_guid  ).

*             Soma o agrupador na formula para o cabeçalho do grupo de movimentos
            ltp_formula_agrp_s1 = ltp_formula_agrp_s1 && '+' && 'E' && ltp_tabix_header.
            ltp_formula_agrp_s2 = ltp_formula_agrp_s2 && '+' && 'F' && ltp_tabix_header.
            ltp_formula_agrp_f1 = ltp_formula_agrp_f1 && '+' && 'H' && ltp_tabix_header.
            ltp_formula_agrp_f2 = ltp_formula_agrp_f2 && '+' && 'I' && ltp_tabix_header.

*             Agrupar linhas de detalhe
            ADD 1 TO ltp_tabix_header.
            o_worksheet->set_row_outline( iv_row_from = ltp_tabix_header iv_row_to = tp_row iv_collapsed = abap_true ).


          ENDIF.
**********************************************************************


        ENDIF.
*       Atualizar somatório do agrupador de movimentos
        IF NOT ltp_formula_agrp_s1 IS INITIAL.
          IF ltp_formula_agrp_s1(1) EQ '+'.
            SHIFT:  ltp_formula_agrp_s1 LEFT BY 1 PLACES,
                    ltp_formula_agrp_s2 LEFT BY 1 PLACES,
                    ltp_formula_agrp_f1 LEFT BY 1 PLACES,
                    ltp_formula_agrp_f2 LEFT BY 1 PLACES.
          ENDIF.
          o_worksheet->set_cell( ip_row = ltp_row_from ip_column = 'E' ip_formula = ltp_formula_agrp_s1 ip_style = tp_style_bold_num_border_guid  ).
          o_worksheet->set_cell( ip_row = ltp_row_from ip_column = 'F' ip_formula = ltp_formula_agrp_s2 ip_style = tp_style_bold_num_border_guid  ).
          o_worksheet->set_cell( ip_row = ltp_row_from ip_column = 'H' ip_formula = ltp_formula_agrp_f1 ip_style = tp_style_bold_num_border_guid  ).
          o_worksheet->set_cell( ip_row = ltp_row_from ip_column = 'I' ip_formula = ltp_formula_agrp_f2 ip_style = tp_style_bold_num_border_guid  ).
        ELSE.
          o_worksheet->set_cell( ip_row = ltp_row_from ip_column = 'E' ip_value = '' ip_style = tp_style_bold_num_border_guid  ).
          o_worksheet->set_cell( ip_row = ltp_row_from ip_column = 'F' ip_value = '' ip_style = tp_style_bold_num_border_guid  ).
          o_worksheet->set_cell( ip_row = ltp_row_from ip_column = 'H' ip_value = '' ip_style = tp_style_bold_num_border_guid  ).
          o_worksheet->set_cell( ip_row = ltp_row_from ip_column = 'I' ip_value = '' ip_style = tp_style_bold_num_border_guid  ).
        ENDIF.

        ltp_formula_doc_sum_s2 = ltp_formula_doc_sum_s2 && '+' && 'F' && ltp_row_from.
        ltp_formula_doc_sum_f2 = ltp_formula_doc_sum_f2 && '+' && 'I' && ltp_row_from.
      ENDLOOP.

*     Totalizador de docs
      ADD 1 TO tp_row.
      IF NOT ltp_formula_doc_sum_s2 IS INITIAL.
        IF ltp_formula_doc_sum_s2(1) EQ '+'.
          SHIFT:  ltp_formula_doc_sum_s2 LEFT BY 1 PLACES,
                  ltp_formula_doc_sum_f2 LEFT BY 1 PLACES.
        ENDIF.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_formula = ltp_formula_doc_sum_s2 ip_style = tp_style_bold_num_border_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_formula = ltp_formula_doc_sum_f2 ip_style = tp_style_bold_num_border_guid  ).

        ltp_formula_tot_view_sum_s2 = ltp_formula_tot_view_sum_s2 && '+' && 'F' && tp_row.
        ltp_formula_tot_view_sum_f2 = ltp_formula_tot_view_sum_f2 && '+' && 'I' && tp_row.
      ENDIF.
*     Total da visão
      ADD 1 TO tp_row.
      IF NOT ltp_formula_tot_view_sum_s2 IS INITIAL.
        IF ltp_formula_tot_view_sum_s2(1) EQ '+'.
          SHIFT:  ltp_formula_tot_view_sum_s2 LEFT BY 1 PLACES,
                  ltp_formula_tot_view_sum_f2 LEFT BY 1 PLACES.
        ENDIF.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_formula = ltp_formula_tot_view_sum_s2 ip_style = tp_style_bold_num_border_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_formula = ltp_formula_tot_view_sum_f2 ip_style = tp_style_bold_num_border_guid  ).

        ltp_formula_tot_file_sum_s2 = ltp_formula_tot_file_sum_s2 && '+' && 'F' && tp_row.
        ltp_formula_tot_file_sum_f2 = ltp_formula_tot_file_sum_f2 && '+' && 'I' && tp_row.
      ENDIF.

**********************************************************************
*     Par inverso
**********************************************************************
      IF NOT wa_t001_partner-bukrs IS INITIAL. "Só mostra se parceiro for empresa
        CLEAR: ltp_formula_doc_sum_s2, ltp_formula_doc_sum_f2, ltp_formula_acc_sum_s1, ltp_formula_acc_sum_s2, ltp_formula_acc_sum_f1, ltp_formula_acc_sum_f2.

        ADD 3 TO tp_row.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = 'Taxa' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = tp_exchange_rate_es ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = 'Taxa' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = tp_exchange_rate_ef ip_style = tp_style_bold_center_bord_guid  ).
        ltp_cell_taxa_s = '$E$' && tp_row.
        ltp_cell_taxa_f = '$H$' && tp_row.

        ADD 1 TO tp_row.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = 'N/CONTAS EM ANÁLISE' ip_style = tp_style_gray125_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = '' ip_style = tp_style_gray125_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_value = '' ip_style = tp_style_gray125_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = '' ip_style = tp_style_gray125_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = '' ip_style = tp_style_gray125_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_value = '' ip_style = tp_style_gray125_guid  ).
        o_worksheet->set_merge( ip_row = tp_row ip_column_start = 'D' ip_column_end = 'I' ).

        ADD 1 TO tp_row.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = 'SALDOS' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_merge( ip_row = tp_row ip_column_start = 'D' ip_column_end = 'F' ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = 'FLUXOS' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_merge( ip_row = tp_row ip_column_start = 'G' ip_column_end = 'I' ).

        ADD 1 TO tp_row.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = 'Conta SNC' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = 'Conta SNC' ip_style = tp_style_bold_center_bord_guid  ).
        CONCATENATE 'Valor ' wa_t001_partner-waers INTO ltp_value_string RESPECTING BLANKS.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = ltp_value_string ip_style = tp_style_bold_center_bord_guid  ).
        CONCATENATE 'Valor ' wa_conct058-moeda_reporte INTO ltp_value_string RESPECTING BLANKS.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_value = ltp_value_string ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = 'Conta SNC' ip_style = tp_style_bold_center_bord_guid  ).
        CONCATENATE 'Valor ' wa_t001_partner-waers INTO ltp_value_string RESPECTING BLANKS.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = ltp_value_string ip_style = tp_style_bold_center_bord_guid  ).
        CONCATENATE 'Valor ' wa_conct058-moeda_reporte INTO ltp_value_string RESPECTING BLANKS.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_value = ltp_value_string ip_style = tp_style_bold_center_bord_guid  ).

        ltp_tabix_config = 1.
        CLEAR: ltp_flag_end_do_s, ltp_flag_end_do_f, lit_balance[].

**********************************************************************
*       Totais por conta
**********************************************************************
        DO.
*       Saldos
          READ TABLE it_conct055_s INTO wa_conct055_s INDEX ltp_tabix_config.
          IF sy-subrc EQ 0.
*         Insere linha de cabeçalho
            CLEAR lwa_balance.
            lwa_balance-header  = abap_true.
            lwa_balance-acc_s   = wa_conct055_s-agrupador.
            APPEND lwa_balance TO lit_balance.
            ltp_tabix_header = sy-tabix. "guarda o indice da linha de header

            ltp_tabix_item = ltp_tabix_header.
*           Pesquisa as contas que respeitam a regra do argupador
            LOOP AT it_saldos_e INTO wa_saldos_e.
              IF wa_saldos_e-racct CP wa_conct055_s-pesquisa.
*             Se tiver o tipo de conta definido, verifica se a conta pertence ao argupador
                IF NOT wa_conct055_s-mitkz IS INITIAL.
*               RSM-ID-18.03-ini
*                SELECT SINGLE mitkz INTO ltp_mitkz FROM skb1 WHERE bukrs = p_bukrs AND saknr = wa_saldos_e-racct.

                  SELECT SINGLE ktopl FROM t001 INTO ltp_ktopl WHERE bukrs = p_bukrs.
                  SELECT            "#EC CI_DB_OPERATION_OK[0002431747]
                  SINGLE mitkz FROM skb1 AS b
                  INNER JOIN        "#EC CI_DB_OPERATION_OK[0002389136]
                   ska1 AS a ON a~saknr = b~saknr
                  INTO ltp_mitkz
                  WHERE b~bukrs = p_bukrs
                  AND b~saknr = wa_saldos_e-racct
                  AND a~ktopl = ltp_ktopl
                  AND a~glaccount_type NE 'S'           "#EC CI_NOORDER
.
*               RSM-ID-18.03-fim
                  IF sy-subrc EQ 0 AND ltp_mitkz NE wa_conct055_s-mitkz.
                    CONTINUE.
                  ENDIF.
                ENDIF.

                CLEAR lwa_balance.
                ADD 1 TO ltp_tabix_item.
                lwa_balance-acc_s = wa_saldos_e-racct.
                lwa_balance-tsl_s = wa_saldos_e-tsl.
                lwa_balance-hsl_s = wa_saldos_e-hsl.
                INSERT lwa_balance INTO lit_balance INDEX ltp_tabix_item.

                DELETE it_saldos_e.
              ENDIF.
            ENDLOOP.
            IF sy-subrc EQ 0 AND ltp_tabix_item GT ltp_tabix_header.
              READ TABLE lit_balance INTO lwa_balance INDEX ltp_tabix_header.

              lwa_balance-count_det   = ltp_tabix_item - ltp_tabix_header.
              MODIFY lit_balance FROM lwa_balance INDEX ltp_tabix_header.
            ENDIF.
          ELSE.
            ltp_flag_end_do_s = abap_true.
          ENDIF.

*         Fluxos
          READ TABLE it_conct055_f INTO wa_conct055_f INDEX ltp_tabix_config.
          IF sy-subrc EQ 0.
*           Altera linha de cabeçalho
            READ TABLE lit_balance INTO lwa_balance INDEX ltp_tabix_header.
            lwa_balance-acc_f   = wa_conct055_f-agrupador.
            MODIFY lit_balance FROM lwa_balance INDEX ltp_tabix_header.

            ltp_tabix_item_f = ltp_tabix_header.
*         Pesquisa as contas que respeitam a regra do argupador
            LOOP AT it_saldos_e_f INTO wa_saldos_e_f.
              IF wa_saldos_e_f-racct CP wa_conct055_f-pesquisa.
*             Se tiver o tipo de conta definido, verifica se a conta pertence ao agrupador
                IF NOT wa_conct055_f-mitkz IS INITIAL.
*               RSM-ID-18.03-ini
*                SELECT SINGLE mitkz INTO ltp_mitkz FROM skb1 WHERE bukrs = p_bukrs AND saknr = wa_saldos_e_f-racct.

                  SELECT SINGLE ktopl FROM t001 INTO ltp_ktopl WHERE bukrs = p_bukrs.
                  SELECT            "#EC CI_DB_OPERATION_OK[0002431747]
                  SINGLE mitkz FROM skb1 AS b
                  INNER JOIN        "#EC CI_DB_OPERATION_OK[0002389136]
                   ska1 AS a ON a~saknr = b~saknr
                  INTO ltp_mitkz
                  WHERE b~bukrs = p_bukrs
                  AND b~saknr = wa_saldos_e_f-racct
                  AND a~ktopl = ltp_ktopl
                  AND a~glaccount_type NE 'S'           "#EC CI_NOORDER
.
*               RSM-ID-18.03-fim
                  IF sy-subrc EQ 0 AND ltp_mitkz NE wa_conct055_f-mitkz.
                    CONTINUE.
                  ENDIF.
                ENDIF.

                CLEAR lwa_balance.
                ADD 1 TO ltp_tabix_item_f.

                IF ltp_tabix_item_f LE ltp_tabix_item. "Saldos já tem conta preenchida, modifica a linha
                  READ TABLE lit_balance INTO lwa_balance  INDEX ltp_tabix_item_f.
                  lwa_balance-acc_f = wa_saldos_e_f-racct.
                  lwa_balance-tsl_f = wa_saldos_e_f-tsl.
                  lwa_balance-hsl_f = wa_saldos_e_f-hsl.
                  MODIFY lit_balance FROM lwa_balance INDEX ltp_tabix_item_f.
                ELSE. "Fluxos tem mais contas do que saldos, acrescenta as linhas extra
                  lwa_balance-acc_f = wa_saldos_e_f-racct.
                  lwa_balance-tsl_f = wa_saldos_e_f-tsl.
                  lwa_balance-hsl_f = wa_saldos_e_f-hsl.
                  INSERT lwa_balance INTO lit_balance INDEX ltp_tabix_item_f.
                ENDIF.

                DELETE it_saldos_e_f.
              ENDIF.
            ENDLOOP.
            IF sy-subrc EQ 0 AND ltp_tabix_item_f GT ltp_tabix_header.
              READ TABLE lit_balance INTO lwa_balance INDEX ltp_tabix_header.
              IF lwa_balance-count_det LT ( ltp_tabix_item_f - ltp_tabix_header ).
                lwa_balance-count_det = ltp_tabix_item_f - ltp_tabix_header .
              ENDIF.
              MODIFY lit_balance FROM lwa_balance INDEX ltp_tabix_header.
            ENDIF.
          ELSE.
            ltp_flag_end_do_f = abap_true.
          ENDIF.

*       Se não encontrar mais registos de saldos e fluxos, sai fora
          IF ltp_flag_end_do_s EQ abap_true AND ltp_flag_end_do_f EQ abap_true.
            EXIT.
          ENDIF.

          ADD 1 TO ltp_tabix_config.
        ENDDO.

**********************************************************************
*       Se ainda tiver contas de saldos que não foram distribuidas pelos agrupadores, coloca em "Outras"
*       Insere linha de cabeçalho
        CLEAR lwa_balance.
        lwa_balance-header  = abap_true.
        lwa_balance-acc_s   = 'Outras'.
        lwa_balance-acc_f   = 'Outras'.
        APPEND lwa_balance TO lit_balance.
        ltp_tabix_header = sy-tabix. "guarda o indice da linha de header

        ltp_tabix_item = ltp_tabix_header.
*       Contas restantes
        LOOP AT it_saldos_e INTO wa_saldos_e.
          CLEAR lwa_balance.
          ADD 1 TO ltp_tabix_item.
          lwa_balance-acc_s = wa_saldos_e-racct.
          lwa_balance-tsl_s = wa_saldos_e-tsl.
          lwa_balance-hsl_s = wa_saldos_e-hsl.
          INSERT lwa_balance INTO lit_balance INDEX ltp_tabix_item.
          DELETE it_saldos_e.
        ENDLOOP.
        IF sy-subrc EQ 0 AND ltp_tabix_item GT ltp_tabix_header.
          READ TABLE lit_balance INTO lwa_balance INDEX ltp_tabix_header.
          lwa_balance-count_det   = ltp_tabix_item - ltp_tabix_header.
          MODIFY lit_balance FROM lwa_balance INDEX ltp_tabix_header.
        ENDIF.
**********************************************************************
*       Se ainda tiver contas de saldos que não foram distribuidas pelos agrupadores, coloca em "Outras"
*       Insere linha de cabeçalho

        ltp_tabix_item_f = ltp_tabix_header.
*       Contas restantes
        LOOP AT it_saldos_e_f INTO wa_saldos_e_f.
          CLEAR lwa_balance.
          ADD 1 TO ltp_tabix_item_f.
          IF ltp_tabix_item_f LE ltp_tabix_item. "Saldos já tem conta preenchida, modifica a linha
            READ TABLE lit_balance INTO lwa_balance  INDEX ltp_tabix_item_f.
            lwa_balance-acc_f = wa_saldos_e_f-racct.
            lwa_balance-tsl_f = wa_saldos_e_f-tsl.
            lwa_balance-hsl_f = wa_saldos_e_f-hsl.
            MODIFY lit_balance FROM lwa_balance INDEX ltp_tabix_item_f.
          ELSE. "Fluxos tem mais contas do que saldos, acrescenta as linhas extra
            lwa_balance-acc_f = wa_saldos_e_f-racct.
            lwa_balance-tsl_f = wa_saldos_e_f-tsl.
            lwa_balance-hsl_f = wa_saldos_e_f-hsl.
            INSERT lwa_balance INTO lit_balance INDEX ltp_tabix_item_f.
          ENDIF.
          DELETE it_saldos_e_f.
        ENDLOOP.
        IF sy-subrc EQ 0 AND ltp_tabix_item_f GT ltp_tabix_header.
          READ TABLE lit_balance INTO lwa_balance INDEX ltp_tabix_header.
          IF lwa_balance-count_det LT ( ltp_tabix_item_f - ltp_tabix_header ).
            lwa_balance-count_det = ltp_tabix_item_f - ltp_tabix_header .
            MODIFY lit_balance FROM lwa_balance INDEX ltp_tabix_header.
          ENDIF.
        ENDIF.
**********************************************************************

*     Preencher excel com base na tabela auxiliar
        ADD 1 TO tp_row.
        ltp_acc_header_count = 0.
        LOOP AT lit_balance INTO lwa_balance.
          CLEAR: ltp_row_from, ltp_row_to, ltp_formula_s1, ltp_formula_s2, ltp_formula_f1, ltp_formula_f2.

          IF lwa_balance-header EQ abap_true.
            ADD 1 TO ltp_acc_header_count.
            IF lwa_balance-count_det GT 0.
              ltp_row_from = tp_row + 1.
              ltp_row_to = tp_row + lwa_balance-count_det.
            ENDIF.

            IF ltp_acc_header_count EQ 1.
              o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = 'EMPRESA EM ANÁLISE: ' ip_style = tp_style_bold_left_guid   ).
              o_worksheet->set_cell( ip_row = tp_row ip_column = 'C' ip_value = wa_t001_partner-butxt ip_style = tp_style_bold_center_guid   ).
            ENDIF.

            IF ltp_acc_header_count EQ 2.
              o_worksheet->set_cell( ip_row = tp_row ip_column = 'C' ip_value = p_partn ip_style = tp_style_bold_center_guid   ).
            ENDIF.

*         Determinar as fórmulas de somatório
            IF lwa_balance-count_det GT 0.
              ltp_formula_s1 = 'SUM(E' && ltp_row_from && ':E' && ltp_row_to && ')'.
              ltp_formula_s2 = 'SUM(F' && ltp_row_from && ':F' && ltp_row_to && ')'.
              ltp_formula_f1 = 'SUM(H' && ltp_row_from && ':H' && ltp_row_to && ')'.
              ltp_formula_f2 = 'SUM(I' && ltp_row_from && ':I' && ltp_row_to && ')'.
            ENDIF.
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value   = lwa_balance-acc_s ip_style  = tp_style_normal_bord_cen_guid  ).
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_formula = ltp_formula_s1 ip_style     = tp_style_number_border_guid  ).
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_formula = ltp_formula_s2 ip_style     = tp_style_number_border_guid  ).
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value   = lwa_balance-acc_f ip_style  = tp_style_normal_bord_cen_guid  ).
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_formula = ltp_formula_f1 ip_style     = tp_style_number_border_guid  ).
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_formula = ltp_formula_f2 ip_style     = tp_style_number_border_guid  ).

*         Somar ao totalizador de contas
            ltp_formula_acc_sum_s1 = ltp_formula_acc_sum_s1 && '+' && 'E' && tp_row.
            ltp_formula_acc_sum_s2 = ltp_formula_acc_sum_s2 && '+' && 'F' && tp_row.
            ltp_formula_acc_sum_f1 = ltp_formula_acc_sum_f1 && '+' && 'H' && tp_row.
            ltp_formula_acc_sum_f2 = ltp_formula_acc_sum_f2 && '+' && 'I' && tp_row.

*         Agrupar linhas de detalhe
            IF lwa_balance-count_det GT 0.
              o_worksheet->set_row_outline( iv_row_from = ltp_row_from iv_row_to = ltp_row_to iv_collapsed = abap_true ).
            ENDIF.
          ELSE.
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = lwa_balance-acc_s ip_style = tp_style_gray_num_guid  ).
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = lwa_balance-hsl_s ip_style = tp_style_gray_num_guid  ).
            ltp_formula_taxa = 'E' && tp_row && '*' && ltp_cell_taxa_s.
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_formula = ltp_formula_taxa ip_style = tp_style_gray_num_guid  ).
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = lwa_balance-acc_f ip_style = tp_style_gray_num_guid  ).
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = lwa_balance-hsl_f ip_style = tp_style_gray_num_guid  ).
            ltp_formula_taxa = 'H' && tp_row && '*' && ltp_cell_taxa_f.
            o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_formula = ltp_formula_taxa ip_style = tp_style_gray_num_guid  ).
          ENDIF.
          ADD 1 TO tp_row.
        ENDLOOP.

*     Totalizador das contas
        IF NOT ltp_formula_acc_sum_s1 IS INITIAL.
          IF ltp_formula_acc_sum_s1(1) EQ '+'.
            SHIFT:  ltp_formula_acc_sum_s1 LEFT BY 1 PLACES,
                    ltp_formula_acc_sum_s2 LEFT BY 1 PLACES,
                    ltp_formula_acc_sum_f1 LEFT BY 1 PLACES,
                    ltp_formula_acc_sum_f2 LEFT BY 1 PLACES.
          ENDIF.
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_formula = ltp_formula_acc_sum_s1 ip_style = tp_style_bold_num_border_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_formula = ltp_formula_acc_sum_s2 ip_style = tp_style_bold_num_border_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_formula = ltp_formula_acc_sum_f1 ip_style = tp_style_bold_num_border_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_formula = ltp_formula_acc_sum_f2 ip_style = tp_style_bold_num_border_guid  ).

          ltp_formula_tot_view_sum_s2 = ltp_formula_tot_view_sum_s2 && '+' && 'F' && tp_row.
          ltp_formula_tot_view_sum_f2 = ltp_formula_tot_view_sum_f2 && '+' && 'I' && tp_row.
        ENDIF.


**********************************************************************
*       Documentos
**********************************************************************

        ADD 1 TO tp_row.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_merge( ip_row = tp_row ip_column_start = 'D' ip_column_end = 'F' ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_value = '' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_merge( ip_row = tp_row ip_column_start = 'G' ip_column_end = 'I' ).


        ADD 1 TO tp_row.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = 'Doc. Nr.' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'B' ip_value = 'Data' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'C' ip_value = 'Movimentos em trânsito' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = 'Conta SNC' ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = 'Conta SNC' ip_style = tp_style_bold_center_bord_guid  ).
        CONCATENATE 'Valor ' wa_t001_partner-waers INTO ltp_value_string RESPECTING BLANKS.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = ltp_value_string ip_style = tp_style_bold_center_bord_guid  ).
        CONCATENATE 'Valor ' wa_conct058-moeda_reporte INTO ltp_value_string RESPECTING BLANKS.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_value = ltp_value_string ip_style = tp_style_bold_center_bord_guid  ).
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = 'Conta SNC' ip_style = tp_style_bold_center_bord_guid  ).
        CONCATENATE 'Valor ' wa_t001_partner-waers INTO ltp_value_string RESPECTING BLANKS.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = ltp_value_string ip_style = tp_style_bold_center_bord_guid  ).
        CONCATENATE 'Valor ' wa_conct058-moeda_reporte INTO ltp_value_string RESPECTING BLANKS.
        o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_value = ltp_value_string ip_style = tp_style_bold_center_bord_guid  ).

        LOOP AT it_conct056 INTO wa_conct056.
          CLEAR: ltp_new_setid, lit_set_values[], lwa_set_values, lwa_r_blart, lit_r_blart[].
          CLEAR: ltp_formula_agrp_s1, ltp_formula_agrp_s2, ltp_formula_agrp_f1, ltp_formula_agrp_f2.

*         Cabeçalho
          ADD 1 TO tp_row.
          ltp_row_from = tp_row.
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = wa_conct056-text_m ip_style = tp_style_normal_border_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'B' ip_value = '' ip_style = tp_style_normal_border_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'C' ip_value = wa_conct056-text_l ip_style = tp_style_normal_border_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = '' ip_style = tp_style_normal_border_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = '' ip_style = tp_style_normal_border_guid  ).


*         Ler nome do set
          CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
            EXPORTING
              shortname                = wa_conct056-setname
            IMPORTING
              new_setid                = ltp_new_setid
            EXCEPTIONS
              no_set_found             = 1
              no_set_picked_from_popup = 2
              wrong_class              = 3
              wrong_subclass           = 4
              table_field_not_found    = 5
              fields_dont_match        = 6
              set_is_empty             = 7
              formula_in_set           = 8
              set_is_dynamic           = 9
              OTHERS                   = 10.
          IF sy-subrc EQ 0.

*         Carregar valores do set
            CALL FUNCTION 'G_SET_GET_ALL_VALUES'
              EXPORTING
                setnr         = ltp_new_setid
              TABLES
                set_values    = lit_set_values
              EXCEPTIONS
                set_not_found = 1
                OTHERS        = 2.
            IF sy-subrc EQ 0.

              LOOP AT lit_set_values INTO lwa_set_values.
                IF wa_conct056-exclude EQ 'X'.
                  lwa_r_blart-sign = 'E'.
                ELSE.
                  lwa_r_blart-sign = 'I'.
                ENDIF.
                IF lwa_set_values-to IS INITIAL OR lwa_set_values-to EQ lwa_set_values-from.
                  lwa_r_blart-option = 'EQ'.
                ELSE.
                  lwa_r_blart-option = 'BT'.
                ENDIF.
                lwa_r_blart-low = lwa_set_values-from.
                lwa_r_blart-high = lwa_set_values-to.
                APPEND lwa_r_blart TO lit_r_blart.
              ENDLOOP.
            ENDIF.
          ENDIF.

*         Percorrer movimentos com TD no set
          IF NOT lit_r_blart IS INITIAL.
*           Dividir por agrupadores de contas
            LOOP AT it_conct055_s INTO wa_conct055_s.
              ltp_tabix_header = tp_row.
              ltp_new_group = 'X'.
              LOOP AT it_mov_e INTO wa_mov_e WHERE sbxc_blart_01 IN lit_r_blart.

                IF wa_mov_e-racct CP wa_conct055_s-pesquisa.
*                 Se tiver o tipo de conta definido, verifica se a conta pertence ao argupador
                  IF NOT wa_conct055_s-mitkz IS INITIAL.
*                   RSM-ID-18.03-ini
*                   SELECT SINGLE mitkz INTO ltp_mitkz FROM skb1 WHERE bukrs = p_bukrs AND saknr = wa_mov_e-racct.

                    SELECT SINGLE ktopl FROM t001 INTO ltp_ktopl WHERE bukrs = p_bukrs.
                    SELECT          "#EC CI_DB_OPERATION_OK[0002431747]
                    SINGLE mitkz FROM skb1 AS b
                    INNER JOIN      "#EC CI_DB_OPERATION_OK[0002389136]
                     ska1 AS a ON a~saknr = b~saknr
                    INTO ltp_mitkz
                    WHERE b~bukrs = p_bukrs
                    AND b~saknr = wa_mov_e-racct
                    AND a~ktopl = ltp_ktopl
                    AND a~glaccount_type NE 'S'         "#EC CI_NOORDER
.
*                   RSM-ID-18.03-fim
                    IF sy-subrc EQ 0 AND ltp_mitkz NE wa_conct055_s-mitkz.
                      CONTINUE.
                    ENDIF.
                  ENDIF.

                  IF ltp_new_group EQ 'X'.
                    ADD 1 TO tp_row. "Reserva linha para o cabeçalho do agrupador de conta
                    ltp_tabix_header = tp_row.
                    CLEAR ltp_new_group.
                  ENDIF.

*                 Escreve documento
                  ADD 1 TO tp_row.
                  o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = wa_mov_e-docnr ip_style = tp_style_gray_guid   ).
                  o_worksheet->set_cell( ip_row = tp_row ip_column = 'B' ip_value = wa_mov_e-budat ip_style = tp_style_gray_guid   ).
                  o_worksheet->set_cell( ip_row = tp_row ip_column = 'C' ip_value = wa_mov_e-description ip_style = tp_style_gray_guid   ).
                  o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = wa_mov_e-racct ip_style = tp_style_gray_guid   ).
                  o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = wa_mov_e-hsl ip_style = tp_style_gray_num_guid   ).
                  ltp_formula_taxa = 'E' && tp_row && '*' && ltp_cell_taxa_s.
                  o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_formula = ltp_formula_taxa ip_style = tp_style_gray_num_guid  ).
*                 Ler fluxos
                  READ TABLE it_mov_e_f INTO lwa_mov_e_f WITH KEY docnr = wa_mov_e-docnr.
                  ltp_index_f = sy-tabix.
                  IF sy-subrc EQ 0.
                    o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = lwa_mov_e_f-racct ip_style = tp_style_gray_guid  ).
                    o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = lwa_mov_e_f-hsl ip_style = tp_style_gray_num_guid   ).
                    ltp_formula_taxa = 'H' && tp_row && '*' && ltp_cell_taxa_f.
                    o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_formula = ltp_formula_taxa ip_style = tp_style_gray_num_guid  ).
                    DELETE it_mov_e_f INDEX ltp_index_f. "Apaga para não listar de novo referente a outra conta de saldos
                  ELSE.
                    o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = '' ip_style = tp_style_gray_guid  ).
                    o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = '' ip_style = tp_style_gray_num_guid   ).
                    o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_value = '' ip_style = tp_style_gray_num_guid ).
                  ENDIF.
                  DELETE it_mov_e. "Apaga para no final ter apenas as contas sem agrupador.
                ENDIF.
              ENDLOOP.
*             Se o agrupador tem movimentos, escreve o cabeçalho do agrupador
              IF ltp_tabix_header LT tp_row.
*               Determinar as fórmulas de somatório
                ADD 1 TO ltp_tabix_header.
                ltp_formula_s1 = 'SUM(E' && ltp_tabix_header && ':E' && tp_row && ')'.
                ltp_formula_s2 = 'SUM(F' && ltp_tabix_header && ':F' && tp_row && ')'.
                ltp_formula_f1 = 'SUM(H' && ltp_tabix_header && ':H' && tp_row && ')'.
                ltp_formula_f2 = 'SUM(I' && ltp_tabix_header && ':I' && tp_row && ')'.
                SUBTRACT 1 FROM ltp_tabix_header.
                o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'A' ip_value = '' ip_style = tp_style_normal_bord_cen_guid    ).
                o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'B' ip_value = '' ip_style = tp_style_normal_bord_cen_guid    ).
                o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'C' ip_value = '' ip_style = tp_style_normal_bord_cen_guid    ).
                o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'D' ip_value   = wa_conct055_s-agrupador ip_style  = tp_style_normal_bord_cen_guid  ).
                o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'E' ip_formula = ltp_formula_s1 ip_style     = tp_style_number_border_guid  ).
                o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'F' ip_formula = ltp_formula_s2 ip_style     = tp_style_number_border_guid  ).
                o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'G' ip_value   = '' ip_style  = tp_style_normal_bord_cen_guid  ).
                o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'H' ip_formula = ltp_formula_f1 ip_style     = tp_style_number_border_guid  ).
                o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'I' ip_formula = ltp_formula_f2 ip_style     = tp_style_number_border_guid  ).

*               Soma o agrupador na formula para o cabeçalho do grupo de movimentos
                ltp_formula_agrp_s1 = ltp_formula_agrp_s1 && '+' && 'E' && ltp_tabix_header.
                ltp_formula_agrp_s2 = ltp_formula_agrp_s2 && '+' && 'F' && ltp_tabix_header.
                ltp_formula_agrp_f1 = ltp_formula_agrp_f1 && '+' && 'H' && ltp_tabix_header.
                ltp_formula_agrp_f2 = ltp_formula_agrp_f2 && '+' && 'I' && ltp_tabix_header.

*               Agrupar linhas de detalhe
                ADD 1 TO ltp_tabix_header.
                o_worksheet->set_row_outline( iv_row_from = ltp_tabix_header iv_row_to = tp_row iv_collapsed = abap_true ).
              ENDIF.

            ENDLOOP.

**********************************************************************
*           Listar contas sem agrupador
            ltp_tabix_header = tp_row.
            ltp_new_group = 'X'.
            LOOP AT it_mov_e INTO wa_mov_e WHERE sbxc_blart_01 IN lit_r_blart.
              IF ltp_new_group EQ 'X'.
                ADD 1 TO tp_row. "Reserva linha para o cabeçalho do agrupador
                ltp_tabix_header = tp_row.
                CLEAR ltp_new_group.
              ENDIF.
*               Escreve documento
              ADD 1 TO tp_row.
              o_worksheet->set_cell( ip_row = tp_row ip_column = 'A' ip_value = wa_mov_e-docnr ip_style = tp_style_gray_guid  ).
              o_worksheet->set_cell( ip_row = tp_row ip_column = 'B' ip_value = wa_mov_e-budat ip_style = tp_style_gray_guid  ).
              o_worksheet->set_cell( ip_row = tp_row ip_column = 'C' ip_value = wa_mov_e-description ip_style = tp_style_gray_guid  ).
              o_worksheet->set_cell( ip_row = tp_row ip_column = 'D' ip_value = wa_mov_e-racct ip_style = tp_style_gray_guid  ).
              o_worksheet->set_cell( ip_row = tp_row ip_column = 'E' ip_value = wa_mov_e-hsl ip_style = tp_style_gray_num_guid   ).
              ltp_formula_taxa = 'E' && tp_row && '*' && ltp_cell_taxa_s.
              o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_formula = ltp_formula_taxa ip_style = tp_style_gray_num_guid  ).
*               Ler fluxos
              READ TABLE it_mov_e_f INTO lwa_mov_e_f WITH KEY docnr = wa_mov_e-docnr.
              ltp_index_f = sy-tabix.
              IF sy-subrc EQ 0.
                o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = lwa_mov_e_f-racct ip_style = tp_style_gray_guid  ).
                o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = lwa_mov_e_f-hsl ip_style = tp_style_gray_num_guid   ).
                ltp_formula_taxa = 'H' && tp_row && '*' && ltp_cell_taxa_f.
                o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_formula = ltp_formula_taxa ip_style = tp_style_gray_num_guid  ).
                DELETE it_mov_e_f INDEX ltp_index_f. "Apaga para não listar de novo referente a outra conta de saldos
              ELSE.
                o_worksheet->set_cell( ip_row = tp_row ip_column = 'G' ip_value = '' ip_style = tp_style_gray_guid  ).
                o_worksheet->set_cell( ip_row = tp_row ip_column = 'H' ip_value = '' ip_style = tp_style_gray_num_guid   ).
                o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_value = '' ip_style = tp_style_gray_num_guid ).
              ENDIF.

              DELETE it_mov_e. "Apaga para no final ter apenas as contas sem agrupador.
            ENDLOOP.
*           Se o agrupador tem movimentos, escreve o cabeçalho do agrupador
            IF ltp_tabix_header LT tp_row.
*             Determinar as fórmulas de somatório
              ADD 1 TO ltp_tabix_header.
              ltp_formula_s1 = 'SUM(E' && ltp_tabix_header && ':E' && tp_row && ')'.
              ltp_formula_s2 = 'SUM(F' && ltp_tabix_header && ':F' && tp_row && ')'.
              ltp_formula_f1 = 'SUM(H' && ltp_tabix_header && ':H' && tp_row && ')'.
              ltp_formula_f2 = 'SUM(I' && ltp_tabix_header && ':I' && tp_row && ')'.
              SUBTRACT 1 FROM ltp_tabix_header.
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'A' ip_value = '' ip_style = tp_style_normal_bord_cen_guid    ).
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'B' ip_value = '' ip_style = tp_style_normal_bord_cen_guid    ).
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'C' ip_value = '' ip_style = tp_style_normal_bord_cen_guid    ).
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'D' ip_value   = 'Outras' ip_style  = tp_style_normal_bord_cen_guid  ).
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'E' ip_formula = ltp_formula_s1 ip_style     = tp_style_number_border_guid  ).
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'F' ip_formula = ltp_formula_s2 ip_style     = tp_style_number_border_guid  ).
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'G' ip_value   = '' ip_style  = tp_style_normal_bord_cen_guid  ).
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'H' ip_formula = ltp_formula_f1 ip_style     = tp_style_number_border_guid  ).
              o_worksheet->set_cell( ip_row = ltp_tabix_header ip_column = 'I' ip_formula = ltp_formula_f2 ip_style     = tp_style_number_border_guid  ).

*             Soma o agrupador na formula para o cabeçalho do grupo de movimentos
              ltp_formula_agrp_s1 = ltp_formula_agrp_s1 && '+' && 'E' && ltp_tabix_header.
              ltp_formula_agrp_s2 = ltp_formula_agrp_s2 && '+' && 'F' && ltp_tabix_header.
              ltp_formula_agrp_f1 = ltp_formula_agrp_f1 && '+' && 'H' && ltp_tabix_header.
              ltp_formula_agrp_f2 = ltp_formula_agrp_f2 && '+' && 'I' && ltp_tabix_header.

*             Agrupar linhas de detalhe
              ADD 1 TO ltp_tabix_header.
              o_worksheet->set_row_outline( iv_row_from = ltp_tabix_header iv_row_to = tp_row iv_collapsed = abap_true ).


            ENDIF.
**********************************************************************

          ENDIF.
*       Atualizar somatório do agrupador de movimentos
          IF NOT ltp_formula_agrp_s1 IS INITIAL.
            IF ltp_formula_agrp_s1(1) EQ '+'.
              SHIFT:  ltp_formula_agrp_s1 LEFT BY 1 PLACES,
                      ltp_formula_agrp_s2 LEFT BY 1 PLACES,
                      ltp_formula_agrp_f1 LEFT BY 1 PLACES,
                      ltp_formula_agrp_f2 LEFT BY 1 PLACES.
            ENDIF.
            o_worksheet->set_cell( ip_row = ltp_row_from ip_column = 'E' ip_formula = ltp_formula_agrp_s1 ip_style = tp_style_bold_num_border_guid  ).
            o_worksheet->set_cell( ip_row = ltp_row_from ip_column = 'F' ip_formula = ltp_formula_agrp_s2 ip_style = tp_style_bold_num_border_guid  ).
            o_worksheet->set_cell( ip_row = ltp_row_from ip_column = 'H' ip_formula = ltp_formula_agrp_f1 ip_style = tp_style_bold_num_border_guid  ).
            o_worksheet->set_cell( ip_row = ltp_row_from ip_column = 'I' ip_formula = ltp_formula_agrp_f2 ip_style = tp_style_bold_num_border_guid  ).
          ELSE.
            o_worksheet->set_cell( ip_row = ltp_row_from ip_column = 'E' ip_value = '' ip_style = tp_style_bold_num_border_guid  ).
            o_worksheet->set_cell( ip_row = ltp_row_from ip_column = 'F' ip_value = '' ip_style = tp_style_bold_num_border_guid  ).
            o_worksheet->set_cell( ip_row = ltp_row_from ip_column = 'H' ip_value = '' ip_style = tp_style_bold_num_border_guid  ).
            o_worksheet->set_cell( ip_row = ltp_row_from ip_column = 'I' ip_value = '' ip_style = tp_style_bold_num_border_guid  ).
          ENDIF.

*       Somar ao totalizador de docs
          ltp_formula_doc_sum_s2 = ltp_formula_doc_sum_s2 && '+' && 'F' && ltp_row_from.
          ltp_formula_doc_sum_f2 = ltp_formula_doc_sum_f2 && '+' && 'I' && ltp_row_from.
        ENDLOOP.
*     Totalizador de docs
        ADD 1 TO tp_row.
        IF NOT ltp_formula_doc_sum_s2 IS INITIAL.
          IF ltp_formula_doc_sum_s2(1) EQ '+'.
            SHIFT:  ltp_formula_doc_sum_s2 LEFT BY 1 PLACES,
                    ltp_formula_doc_sum_f2 LEFT BY 1 PLACES.
          ENDIF.
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_formula = ltp_formula_doc_sum_s2 ip_style = tp_style_bold_num_border_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_formula = ltp_formula_doc_sum_f2 ip_style = tp_style_bold_num_border_guid  ).

          ltp_formula_tot_view_sum_s2 = ltp_formula_tot_view_sum_s2 && '+' && 'F' && tp_row.
          ltp_formula_tot_view_sum_f2 = ltp_formula_tot_view_sum_f2 && '+' && 'I' && tp_row.
        ENDIF.

*     Total da visão
        ADD 1 TO tp_row.
        IF NOT ltp_formula_tot_view_sum_s2 IS INITIAL.
          IF ltp_formula_tot_view_sum_s2(1) EQ '+'.
            SHIFT:  ltp_formula_tot_view_sum_s2 LEFT BY 1 PLACES,
                    ltp_formula_tot_view_sum_f2 LEFT BY 1 PLACES.
          ENDIF.
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_formula = ltp_formula_tot_view_sum_s2 ip_style = tp_style_bold_num_border_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_formula = ltp_formula_tot_view_sum_f2 ip_style = tp_style_bold_num_border_guid  ).

          ltp_formula_tot_file_sum_s2 = ltp_formula_tot_file_sum_s2 && '+' && 'F' && tp_row.
          ltp_formula_tot_file_sum_f2 = ltp_formula_tot_file_sum_f2 && '+' && 'I' && tp_row.
        ENDIF.

*     Total do ficheiro
        ADD 2 TO tp_row.
        IF NOT ltp_formula_tot_file_sum_s2 IS INITIAL.
          IF ltp_formula_tot_file_sum_s2(1) EQ '+'.
            SHIFT:  ltp_formula_tot_file_sum_s2 LEFT BY 1 PLACES,
                    ltp_formula_tot_file_sum_f2 LEFT BY 1 PLACES.
          ENDIF.
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'F' ip_formula = ltp_formula_tot_file_sum_s2 ip_style = tp_style_bold_num_border_guid  ).
          o_worksheet->set_cell( ip_row = tp_row ip_column = 'I' ip_formula = ltp_formula_tot_file_sum_f2 ip_style = tp_style_bold_num_border_guid  ).

        ENDIF.

      ENDIF. "Visão parceiro

*     Inserir logo
      IF NOT wa_conct058-relid IS INITIAL AND NOT wa_conct058-objid IS INITIAL.
        o_drawing = o_excel->add_new_drawing( ).
        o_drawing->set_position( ip_from_row = tp_row
                                  ip_from_col = 'A' ).

        wa_key-relid = wa_conct058-relid.
        wa_key-objid = wa_conct058-objid.
        IF NOT wa_conct058-img_width IS INITIAL AND NOT wa_conct058-img_height IS INITIAL.
          o_drawing->set_media_www( ip_key = wa_key ip_width = wa_conct058-img_width ip_height = wa_conct058-img_height ).
        ENDIF.

        o_worksheet->add_drawing( o_drawing ).
      ENDIF.


    CATCH zcx_excel INTO x_excel.
      ltp_message = x_excel->get_text( ).
      MESSAGE ltp_message TYPE 'E'.
  ENDTRY.
ENDFORM.                    "create_worksheet_content
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*       Download do ficheiro
*----------------------------------------------------------------------*
FORM download_file .
  DATA: ltp_filename TYPE string.
  ltp_filename = p_path_f.
*  IF p_path_f IS INITIAL.
*    p_path_f = tp_workdir.
*  ENDIF.
*  cl_gui_frontend_services=>get_file_separator( CHANGING file_separator = tp_file_separator ).
*  CONCATENATE p_path_f tp_file_separator tp_defauit_file_name INTO tp_full_path.

* Criar o objeto de conversão (Excel writer versão 2007)
  CREATE OBJECT o_excel_writer TYPE zcl_excel_writer_2007.
* Converter o objeto excel numa string com um XML
  tp_file = o_excel_writer->write_file( o_excel ).


* Converter string em binário ( SAP_ABA < 6.40 )
  it_file_tab = cl_bcs_convert=>xstring_to_solix( iv_xstring  = tp_file ).
  tp_bytecount = xstrlen( tp_file ).

* Download para o frontend
*  cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = tp_bytecount
*                                                    filename     = tp_full_path
*                                                    filetype     = 'BIN'
*                                           CHANGING data_tab     = it_file_tab ).

  IF r_down EQ 'X' AND NOT p_path_f IS INITIAL.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        bin_filesize            = tp_bytecount
*       filename                = tp_full_path
        filename                = ltp_filename
        filetype                = 'BIN'
      CHANGING
        data_tab                = it_file_tab
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.
    IF sy-subrc <> 0.
      MESSAGE 'Erro ao gravar ficheiro. P.f. verifique se está aberto no Excel' TYPE 'E'.
    ENDIF.
  ENDIF.
  IF r_back EQ 'X' AND NOT p_path_b IS INITIAL.
    DATA: bytes_remain TYPE i.
    FIELD-SYMBOLS: <rawdata> LIKE LINE OF it_file_tab.

    OPEN DATASET p_path_b FOR OUTPUT IN BINARY MODE.
    CHECK sy-subrc = 0.

    bytes_remain = tp_bytecount.

    LOOP AT it_file_tab ASSIGNING <rawdata>.

      AT LAST.
        CHECK bytes_remain >= 0.
        TRANSFER <rawdata> TO p_path_b LENGTH bytes_remain.
        EXIT.
      ENDAT.

      TRANSFER <rawdata> TO p_path_b.
      SUBTRACT 255 FROM bytes_remain.  " Solix has length 255

    ENDLOOP.

    CLOSE DATASET p_path_b.

  ENDIF.

ENDFORM.                    "download_file
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  DATA: ltp_flows TYPE /sbxc/conce079. "RSM-ID-18.20-ins
  PERFORM determina_datas CHANGING tp_budati tp_budatf.
  PERFORM get_report_settings.
  PERFORM get_company_data.
  PERFORM get_partner_data.
  PERFORM get_movs_s.
* RSM-ID-18.20-ini
  SELECT SINGLE sbxc_flows FROM /sbxc/conct004 INTO ltp_flows WHERE bu_partner = p_partn.
  IF ltp_flows EQ abap_true.
* RSM-ID-18.20-fim
    PERFORM get_movs_f.
  ENDIF. "RSM-ID-18.20-ins
  PERFORM get_currency.
  PERFORM merge_movs.
*  PERFORM handle_balance_tab.
ENDFORM.                    "get_data
*&---------------------------------------------------------------------*
*&      Form  GET_COMPANY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_company_data .

  SELECT SINGLE * FROM t001 INTO CORRESPONDING FIELDS OF wa_t001
  WHERE bukrs = p_bukrs.

  CHECK: sy-subrc EQ 0, NOT wa_t001-adrnr IS INITIAL.

  SELECT SINGLE * FROM adrc INTO CORRESPONDING FIELDS OF wa_adrc
  WHERE addrnumber = wa_t001-adrnr.                         "#EC WARNOK

ENDFORM.                    "get_company_data
*&---------------------------------------------------------------------*
*&      Form  GET_PARTNER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_partner_data .
*RSM-ID-18.11-ini
*  DATA: wa_/sbxc/conct033 TYPE /sbxc/conct033.
*
*  SELECT SINGLE *
*    FROM /sbxc/conct033
*    INTO wa_/sbxc/conct033
*    WHERE bu_partner = p_partn.                             "#EC WARNOK
*
*  IF sy-subrc NE 0.
*    MESSAGE 'Parceiro não encontrado' TYPE 'E'.
*  ENDIF.
*
*  SELECT SINGLE name1
*    FROM t880
*    INTO wa_t001_partner-butxt
*    WHERE rcomp = wa_/sbxc/conct033-rcomp.
*  IF sy-subrc NE 0.
*    MESSAGE 'Parceiro não encontrado' TYPE 'E'.
*  ENDIF.
*
** Verifica se o parceiro é empresa
*  CLEAR wa_t001_partner.
*  SELECT SINGLE * FROM t001 INTO wa_t001_partner
*    WHERE rcomp =  wa_/sbxc/conct033-rcomp .                "#EC WARNOK
  /sbxc/cl_conc_bp_cvi=>get_company_by_bp(
    EXPORTING
      i_tp_partner           = p_partn
    IMPORTING
      e_tp_company_code      = DATA(ltp_partner_bukrs) ).

  SELECT SINGLE * FROM t001 INTO wa_t001_partner
  WHERE bukrs = ltp_partner_bukrs.
*RSM-ID-18.11-fim
ENDFORM.                    "get_partner_data
*&---------------------------------------------------------------------*
*&      Form  GET_MOVS_S
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_movs_s .
  TYPES: BEGIN OF lty_saldos_conta,
           racct TYPE racct,
           hsl   TYPE /sbxc/conce096,
           tsl   TYPE /sbxc/conce096,
           rtcur TYPE rtcur,
         END OF lty_saldos_conta,
         lty_t_saldos_conta TYPE STANDARD TABLE OF lty_saldos_conta.

  DATA: t_conct002         TYPE TABLE OF /sbxc/conct002 WITH HEADER LINE,
*        t_t881             TYPE TABLE OF t881 WITH HEADER LINE,    "RSM-ID-18.05-del
*        t_dd03l            TYPE TABLE OF dd03l WITH HEADER LINE,   "RSM-ID-18.05-del
*        ncaract            TYPE i,                                 "RSM-ID-18.05-del
        t_fcttabint        TYPE lvc_t_fcat,
        t_fcttabext        TYPE lvc_t_fcat,
        w_fcttabint        TYPE lvc_s_fcat,
        w_fcttabext        TYPE lvc_s_fcat,
        dyn_internos       TYPE REF TO data,
        dyn_externos       TYPE REF TO data,
        dyn_winternos      TYPE REF TO data,
        dyn_wexternos      TYPE REF TO data,
        d_internos_lista   TYPE REF TO data,
        d_externos_lista   TYPE REF TO data,
        w_internos_lista   TYPE REF TO data,
        w_externos_lista   TYPE REF TO data,
        wa_saldos_i        TYPE /sbxc/concs035,
        wa_saldos_e        TYPE /sbxc/concs036,
        sbxc_perio         TYPE /sbxc/conce001,
        l_tabix            TYPE sy-tabix,
        l_subrc            TYPE sy-subrc,
        l_sort(20),
        l_action           TYPE /sbxc/conce099,
        lwa_saldos_conta_i TYPE lty_saldos_conta,
        lwa_saldos_conta_e TYPE lty_saldos_conta,
        lt_saldos_conta_i  TYPE lty_t_saldos_conta,
        lt_saldos_conta_e  TYPE lty_t_saldos_conta,
        lwa_skat           TYPE skat,
        lwa_t001           TYPE t001.
*        lwa_conct033       TYPE /sbxc/conct033. "RSM-18.11-del
  DATA: lit_saldos_i TYPE TABLE OF /sbxc/concs035,
        lit_saldos_e TYPE TABLE OF /sbxc/concs036.
  FIELD-SYMBOLS: <t_internos>       TYPE STANDARD TABLE,
                 <t_externos>       TYPE STANDARD TABLE,
                 <t_internos_lista> TYPE STANDARD TABLE,
                 <t_externos_lista> TYPE STANDARD TABLE,
                 <sbxc_conci>       TYPE any,
                 <sbxc_st_nx>       TYPE any,
                 <rldnr>            TYPE any,
                 <w_internos>       TYPE any,
                 <w_externos>       TYPE any,
                 <w_internos_lista> TYPE any,
                 <w_externos_lista> TYPE any,
                 <gl_sirid>         TYPE any,
                 <rsbxc_vbund_01>   TYPE any,
                 <rsbxc_lifnr_01>   TYPE any,
                 <rsbxc_kunnr_01>   TYPE any,
                 <sbxc_xblnr_01>    TYPE any,
                 <sbxc_bktxt_01>    TYPE any,
                 <rbukrs>           TYPE any,
                 <racct>            TYPE any,
                 <docnr>            TYPE any,
                 <budat>            TYPE any,
                 <tsl>              TYPE any,
                 <hsl>              TYPE any,
                 <rtcur>            TYPE any,
                 <refdocnr>         TYPE any,
                 <awtyp>            TYPE any,
                 <aworg>            TYPE any,
                 <activ>            TYPE any,
                 <awkey>            TYPE any,
                 <rclnt>            TYPE any,
                 <sbxc_blart_01>    TYPE any.
* RSM-ID-18.05-ini
*  CLEAR:   t_conct002, t_t881, t_dd03l.
*  REFRESH: t_conct002, t_t881, t_dd03l.
*  SELECT * FROM /sbxc/conct002 CLIENT SPECIFIED INTO CORRESPONDING FIELDS OF TABLE t_conct002 WHERE mandt = sy-mandt.
*  SELECT * FROM t881 INTO CORRESPONDING FIELDS OF TABLE t_t881 FOR ALL ENTRIES IN t_conct002
*  WHERE rldnr  = t_conct002-sbxc_rldnri
*  OR rldnr = t_conct002-sbxc_rldnrfi.
*  LOOP AT t_t881.
*    ncaract = strlen( t_t881-tab ) - 1.
*    CONCATENATE t_t881-tab(ncaract) 'A' INTO t_t881-tab.
*    MODIFY t_t881.
*  ENDLOOP.
*  IF NOT t_t881[] IS INITIAL.
*    SELECT DISTINCT * FROM dd03l INTO CORRESPONDING FIELDS OF TABLE t_dd03l FOR ALL ENTRIES IN t_t881
*    WHERE tabname = t_t881-tab.
*  ENDIF.
*  SORT t_dd03l BY position tabname.
*  REFRESH: t_fcttabint.
  DATA: lit_dd03l TYPE TABLE OF dd03l,
        lwa_dd03l TYPE dd03l.

  CLEAR: lit_dd03l[], lwa_dd03l, t_conct002, t_conct002[], t_fcttabint[].
  lit_dd03l = /sbxc/cl_conc_generic_cust=>get_all_ledger_fields(
               i_tp_internal = abap_true
               i_tp_external = abap_false  ).

* RSM-ID-18.05-fim
  PERFORM adicionar_elemento USING t_fcttabint '/SBXC/CONCS012' 'BOX'.
  PERFORM adicionar_elemento USING t_fcttabint '/SBXC/CONCS012' 'ROWCOLOR' .
  PERFORM adicionar_elemento USING t_fcttabint '/SBXC/CONCS012' 'BUTTON' .
  PERFORM adicionar_elemento USING t_fcttabint '/SBXC/CONCS012' 'SBXC_COMENT' .
  PERFORM adicionar_elemento USING t_fcttabint '/SBXC/CONCS012' 'SBXC_ST' .
  PERFORM adicionar_elemento USING t_fcttabint '/SBXC/CONCS012' 'SBXC_ST_NX' .
  PERFORM adicionar_elemento USING t_fcttabint '/SBXC/CONCS012' 'CELLSTYLE'.
  PERFORM adicionar_elemento USING t_fcttabint '/SBXC/CONCS012' 'SBXC_PERVAL'.
* RSM-ID-18.05-ini
*  LOOP AT t_dd03l WHERE rollname <> ''.
*    READ TABLE t_fcttabint INTO w_fcttabint WITH KEY fieldname = t_dd03l-fieldname.
*    IF sy-subrc NE 0.
*      PERFORM adicionar_elemento USING t_fcttabint t_dd03l-tabname t_dd03l-fieldname.
*    ENDIF.
*  ENDLOOP.
  LOOP AT lit_dd03l INTO lwa_dd03l WHERE rollname <> ''.
    READ TABLE t_fcttabint INTO w_fcttabint WITH KEY fieldname = lwa_dd03l-fieldname.
    IF sy-subrc NE 0.
      PERFORM adicionar_elemento USING t_fcttabint lwa_dd03l-tabname lwa_dd03l-fieldname.
    ENDIF.
  ENDLOOP.
* RSM-ID-18.05-fim
  PERFORM adicionar_estrutura USING '/SBXC/CONCS011' t_fcttabint.
  PERFORM adicionar_elemento USING t_fcttabint 'T882' 'CURR1'.
  PERFORM adicionar_elemento USING t_fcttabint 'T882' 'CURR2'.
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = t_fcttabint
    IMPORTING
      ep_table        = dyn_internos.
  ASSIGN dyn_internos->* TO <t_internos>.
  CREATE DATA dyn_winternos LIKE LINE OF <t_internos>.
  ASSIGN dyn_winternos->* TO <w_internos>.

* RSM-ID-18.05-ini
*  CLEAR:   t_conct002, t_t881, t_dd03l.
*  REFRESH: t_conct002, t_t881, t_dd03l.
*  SELECT * FROM /sbxc/conct002 CLIENT SPECIFIED INTO CORRESPONDING
*  FIELDS OF TABLE t_conct002  WHERE mandt = sy-mandt.
*  SELECT * FROM t881 INTO CORRESPONDING FIELDS OF TABLE t_t881 FOR ALL
*  ENTRIES IN t_conct002
*  WHERE rldnr  = t_conct002-sbxc_rldnre
*  OR rldnr  = t_conct002-sbxc_rldnrfe.
*  LOOP AT t_t881.
*    ncaract = strlen( t_t881-tab ) - 1.
*    CONCATENATE t_t881-tab(ncaract) 'A' INTO t_t881-tab.
*    MODIFY t_t881.
*  ENDLOOP.
*  IF NOT t_t881[] IS INITIAL.
*    SELECT DISTINCT * FROM dd03l INTO CORRESPONDING FIELDS OF TABLE
*  t_dd03l FOR ALL ENTRIES IN t_t881 WHERE tabname = t_t881-tab.
*  ENDIF.
*  SORT t_dd03l BY position tabname.
*  REFRESH: t_fcttabext.
  CLEAR: lit_dd03l[], lwa_dd03l, t_conct002, t_conct002[], t_fcttabext[].
  lit_dd03l = /sbxc/cl_conc_generic_cust=>get_all_ledger_fields(
               i_tp_internal = abap_false
               i_tp_external = abap_true  ).
* RSM-ID-18.05-fim

  PERFORM adicionar_elemento USING t_fcttabext '/SBXC/CONCS012' 'BOX'.
  PERFORM adicionar_elemento USING t_fcttabext '/SBXC/CONCS012' 'ROWCOLOR' .
  PERFORM adicionar_elemento USING t_fcttabext '/SBXC/CONCS012' 'BUTTON' .
  PERFORM adicionar_elemento USING t_fcttabext '/SBXC/CONCS012' 'SBXC_COMENT' .
  PERFORM adicionar_elemento USING t_fcttabext '/SBXC/CONCS012' 'SBXC_ST' .
  PERFORM adicionar_elemento USING t_fcttabext '/SBXC/CONCS012' 'SBXC_ST_NX' .
  PERFORM adicionar_elemento USING t_fcttabext '/SBXC/CONCS012' 'CELLSTYLE'.
  PERFORM adicionar_elemento USING t_fcttabext '/SBXC/CONCS012' 'SBXC_PERVAL'.
* RSM-ID-18.05-ini
*  LOOP AT t_dd03l WHERE rollname <> ''.
*    READ TABLE t_fcttabext INTO w_fcttabext WITH KEY fieldname = t_dd03l-fieldname.
*    IF sy-subrc NE 0.
*      PERFORM adicionar_elemento USING t_fcttabext t_dd03l-tabname t_dd03l-fieldname.
*    ENDIF.
*  ENDLOOP.
  LOOP AT lit_dd03l INTO lwa_dd03l WHERE rollname <> ''.
    READ TABLE t_fcttabext INTO w_fcttabext WITH KEY fieldname = lwa_dd03l-fieldname.
    IF sy-subrc NE 0.
      PERFORM adicionar_elemento USING t_fcttabext lwa_dd03l-tabname lwa_dd03l-fieldname.
    ENDIF.
  ENDLOOP.
* RSM-ID-18.05-fim

  PERFORM adicionar_estrutura USING '/SBXC/CONCS011' t_fcttabext.
  PERFORM adicionar_elemento USING t_fcttabext 'T882' 'CURR1'.
  PERFORM adicionar_elemento USING t_fcttabext 'T882' 'CURR2'.
*Gerar Tabela comunicação itens internos
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = t_fcttabext
    IMPORTING
      ep_table        = dyn_externos.
  ASSIGN dyn_externos->* TO <t_externos>.
  CREATE DATA dyn_wexternos LIKE LINE OF <t_externos>.
  ASSIGN dyn_wexternos->* TO <w_externos>.

  SELECT SINGLE sbxc_perio_01 FROM /sbxc/conct004
                              INTO sbxc_perio
                             WHERE bu_partner = p_partn.
  CREATE DATA d_internos_lista LIKE TABLE OF <w_internos>.
  CREATE DATA d_externos_lista LIKE TABLE OF <w_externos>.
  ASSIGN d_internos_lista->* TO <t_internos_lista>.
  ASSIGN d_externos_lista->* TO <t_externos_lista>.
  CREATE DATA w_internos_lista LIKE LINE OF <t_internos_lista>.
  CREATE DATA w_externos_lista LIKE LINE OF <t_externos_lista>.
  ASSIGN w_internos_lista->*   TO <w_internos_lista>.
  ASSIGN w_externos_lista->*   TO <w_externos_lista>.

  CLEAR: lit_saldos_i, lit_saldos_e.
  REFRESH: lit_saldos_i, lit_saldos_e,
           <t_internos>, <t_externos>,
           <t_internos_lista>, <t_externos_lista>.

  CALL FUNCTION '/SBXC/CONC_LERMOVIMENTOS'
    EXPORTING
      i_bukrs       = p_bukrs
      i_partner     = p_partn
      i_datumi      = tp_budati
      i_datumf      = tp_budatf
      sbxc_perio    = sbxc_perio
      i_fluxos      = ''
      l_pend        = '1'
      l_prm         = '1'
      l_pra         = '1'
      l_cnm         = '1'
      l_cna         = '1'
    TABLES
      t_itens_i     = <t_internos>
      t_itens_e     = <t_externos>
      t_itens_i_all = <t_internos_lista>
      t_itens_e_all = <t_externos_lista>.
  CALL FUNCTION '/SBXC/CONC_LERSALDO'
    EXPORTING
      i_bukrs    = p_bukrs
      i_partner  = p_partn
      i_datumi   = tp_budati
      i_datumf   = tp_budatf
      i_fluxos   = ''
      sbxc_perio = sbxc_perio
      l_pend     = '1'
      l_prm      = '0'
      l_pra      = '0'
      l_cnm      = '0'
      l_cna      = '0'
      l_cnt      = 'X'
      l_smd      = 'X'
    TABLES
      t_saldo_i  = lit_saldos_i
      t_saldo_e  = lit_saldos_e
      t_itens_i  = <t_internos_lista>
      t_itens_e  = <t_externos_lista>.

  ASSIGN COMPONENT 'RBUKRS' OF STRUCTURE <w_externos> TO <rbukrs>.
  LOOP AT <t_externos> INTO <w_externos>.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM t001 INTO CORRESPONDING FIELDS OF lwa_t001
    WHERE bukrs = <rbukrs>.
  ELSE.
* RSM-ID-18.11-ini
*    SELECT * FROM /sbxc/conct033 INTO lwa_conct033 WHERE bu_partner = p_partn.
*      SELECT SINGLE * FROM t001 INTO CORRESPONDING FIELDS OF lwa_t001
*      WHERE rcomp = lwa_conct033-rcomp.                     "#EC WARNOK
*      IF sy-subrc EQ 0.
*        EXIT.
*      ENDIF.
*    ENDSELECT.
    /sbxc/cl_conc_bp_cvi=>get_company_by_bp(
      EXPORTING
        i_tp_partner           = p_partn
      IMPORTING
        e_tp_company_code      = DATA(ltp_bukrs)  ).
    SELECT SINGLE * FROM t001 INTO lwa_t001 WHERE bukrs = ltp_bukrs.
* RSM-ID-18.11-fim
  ENDIF.

  LOOP AT lit_saldos_i INTO wa_saldos_i
  WHERE tipo = 3.
    COLLECT wa_saldos_i INTO it_saldos_i.
  ENDLOOP.
  SORT it_saldos_i.
  LOOP AT lit_saldos_e INTO wa_saldos_e
  WHERE tipo = 3.
    COLLECT wa_saldos_e INTO it_saldos_e.
  ENDLOOP.
  SORT it_saldos_i.


*Tratar correções
  ASSIGN COMPONENT 'GL_SIRID'   OF STRUCTURE <w_internos> TO <gl_sirid>.
  ASSIGN COMPONENT 'SBXC_CONCI' OF STRUCTURE <w_internos> TO <sbxc_conci>.
  ASSIGN COMPONENT 'SBXC_ST_NX' OF STRUCTURE <w_internos> TO <sbxc_st_nx>.
  ASSIGN COMPONENT 'RLDNR' OF STRUCTURE <w_internos> TO <rldnr>.

  READ TABLE t_conct002 WITH KEY sbxc_perio = sbxc_perio.
  CHECK sy-subrc EQ 0.
  LOOP AT <t_internos> INTO <w_internos>.
    l_tabix = sy-tabix.
    IF <sbxc_conci> = '' OR  <sbxc_st_nx> = 'X'.
      CHECK: <rldnr> = t_conct002-sbxc_rldnri OR <rldnr> = t_conct002-sbxc_rldnrfi.
      SELECT SINGLE action FROM /sbxc/conct036 INTO l_action
      WHERE gl_sirid = <gl_sirid> AND action = '1'.
      CHECK sy-subrc NE 0.

      MOVE-CORRESPONDING <w_internos> TO <w_externos>.
      APPEND <w_externos> TO <t_externos>.
      DELETE <t_internos> INDEX l_tabix.
    ELSE.
      DELETE <t_internos> INDEX l_tabix.
    ENDIF.
  ENDLOOP.
  ASSIGN COMPONENT 'GL_SIRID' OF STRUCTURE <w_externos> TO <gl_sirid>.
  ASSIGN COMPONENT 'SBXC_CONCI' OF STRUCTURE <w_externos> TO <sbxc_conci>.
  ASSIGN COMPONENT 'SBXC_ST_NX' OF STRUCTURE <w_externos> TO <sbxc_st_nx>.
  ASSIGN COMPONENT 'RLDNR' OF STRUCTURE <w_externos> TO <rldnr>.
  LOOP AT <t_externos> INTO <w_externos>.
    l_tabix = sy-tabix.
    IF <sbxc_conci> = '' OR  <sbxc_st_nx> = 'X'.
      CHECK: <rldnr> = t_conct002-sbxc_rldnre OR <rldnr> = t_conct002-sbxc_rldnrfe.
      SELECT SINGLE action FROM /sbxc/conct036 INTO l_action
      WHERE gl_sirid = <gl_sirid> AND action = '1'.
      CHECK sy-subrc NE 0.

      MOVE-CORRESPONDING <w_externos> TO <w_internos>.
      APPEND <w_internos> TO <t_internos>.
      DELETE <t_externos> INDEX l_tabix.
    ELSE.
      DELETE <t_externos> INDEX l_tabix.
    ENDIF.
  ENDLOOP.

  ASSIGN COMPONENT 'RSBXC_VBUND_01' OF STRUCTURE <w_internos> TO  <rsbxc_vbund_01>.
  ASSIGN COMPONENT 'RSBXC_KUNNR_01' OF STRUCTURE <w_internos> TO  <rsbxc_kunnr_01>.
  ASSIGN COMPONENT 'RSBXC_LIFNR_01' OF STRUCTURE <w_internos> TO  <rsbxc_lifnr_01>.
  ASSIGN COMPONENT 'RACCT' OF STRUCTURE <w_internos> TO <racct>.
  ASSIGN COMPONENT 'SBXC_XBLNR_01' OF STRUCTURE <w_internos> TO  <sbxc_xblnr_01>.
  ASSIGN COMPONENT 'SBXC_BKTXT_01' OF STRUCTURE <w_internos> TO  <sbxc_bktxt_01>.
  ASSIGN COMPONENT 'DOCNR' OF STRUCTURE <w_internos> TO <docnr>.
  ASSIGN COMPONENT 'BUDAT' OF STRUCTURE <w_internos> TO <budat>.
  ASSIGN COMPONENT 'TSL' OF STRUCTURE <w_internos> TO <tsl>.
  ASSIGN COMPONENT 'HSL' OF STRUCTURE <w_internos> TO <hsl>.
  ASSIGN COMPONENT 'RTCUR' OF STRUCTURE <w_internos> TO <rtcur>.
  ASSIGN COMPONENT 'REFDOCNR' OF STRUCTURE <w_internos> TO <refdocnr>.
  ASSIGN COMPONENT 'AWTYP' OF STRUCTURE <w_internos> TO <awtyp>.
  ASSIGN COMPONENT 'AWORG' OF STRUCTURE <w_internos> TO <aworg>.
  ASSIGN COMPONENT 'GL_SIRID'   OF STRUCTURE <w_internos> TO <gl_sirid>.
  ASSIGN COMPONENT 'SBXC_CONCI' OF STRUCTURE <w_internos> TO <sbxc_conci>.
  ASSIGN COMPONENT 'SBXC_ST_NX' OF STRUCTURE <w_internos> TO <sbxc_st_nx>.
  ASSIGN COMPONENT 'ACTIV'      OF STRUCTURE <w_internos> TO <activ>.
  ASSIGN COMPONENT 'AWKEY'      OF STRUCTURE <w_internos> TO <awkey>.
  ASSIGN COMPONENT 'RBUKRS'     OF STRUCTURE <w_internos> TO <rbukrs>.
  ASSIGN COMPONENT 'RCLNT'      OF STRUCTURE <w_internos> TO <rclnt>.
  ASSIGN COMPONENT 'SBXC_BLART_01' OF STRUCTURE <w_internos> TO <sbxc_blart_01>.


  LOOP AT <t_internos> INTO <w_internos>.
    CHECK <sbxc_conci> = '' OR  <sbxc_st_nx> = 'X'.
    CLEAR: wa_mov_i.
    wa_mov_i-racct          = <racct>.
    wa_mov_i-description    = <sbxc_bktxt_01>.

    PERFORM determine_fi_doc  USING <awtyp>
                                    <refdocnr>
                                    <aworg>
                                    <rbukrs>
                                    <rclnt>
                           CHANGING wa_mov_i-docnr .

    wa_mov_i-budat = <budat>.
    wa_mov_i-tsl = - <tsl>.
    wa_mov_i-hsl = - <hsl>.
    wa_mov_i-rtcur = <rtcur>.
    wa_mov_i-sbxc_blart_01 = <sbxc_blart_01>.
    APPEND wa_mov_i TO it_mov_i.
    DELETE <t_internos>.
  ENDLOOP.

  SORT it_mov BY budat_i.
  l_sort = 'BUDAT'.
  SORT <t_externos> BY (l_sort).
  ASSIGN COMPONENT 'RSBXC_VBUND_01' OF STRUCTURE <w_externos> TO  <rsbxc_vbund_01>.
  ASSIGN COMPONENT 'RSBXC_KUNNR_01' OF STRUCTURE <w_externos> TO  <rsbxc_kunnr_01>.
  ASSIGN COMPONENT 'RSBXC_LIFNR_01' OF STRUCTURE <w_externos> TO  <rsbxc_lifnr_01>.
  ASSIGN COMPONENT 'RACCT' OF STRUCTURE <w_externos> TO <racct>.
  ASSIGN COMPONENT 'SBXC_XBLNR_01' OF STRUCTURE <w_externos> TO  <sbxc_xblnr_01>.
  ASSIGN COMPONENT 'SBXC_BKTXT_01' OF STRUCTURE <w_externos> TO  <sbxc_bktxt_01>.
  ASSIGN COMPONENT 'DOCNR' OF STRUCTURE <w_externos> TO <docnr>.
  ASSIGN COMPONENT 'BUDAT' OF STRUCTURE <w_externos> TO <budat>.
  ASSIGN COMPONENT 'TSL' OF STRUCTURE <w_externos> TO <tsl>.
  ASSIGN COMPONENT 'HSL' OF STRUCTURE <w_externos> TO <hsl>.
  ASSIGN COMPONENT 'RTCUR' OF STRUCTURE <w_externos> TO <rtcur>.
  ASSIGN COMPONENT 'REFDOCNR' OF STRUCTURE <w_externos> TO <refdocnr>.
  ASSIGN COMPONENT 'AWTYP' OF STRUCTURE <w_externos> TO <awtyp>.
  ASSIGN COMPONENT 'AWORG' OF STRUCTURE <w_externos> TO <aworg>.
  ASSIGN COMPONENT 'GL_SIRID'   OF STRUCTURE <w_externos> TO <gl_sirid>.
  ASSIGN COMPONENT 'ACTIV'      OF STRUCTURE <w_externos> TO <activ>.
  ASSIGN COMPONENT 'AWKEY'      OF STRUCTURE <w_externos> TO <awkey>.
  ASSIGN COMPONENT 'RBUKRS'     OF STRUCTURE <w_externos> TO <rbukrs>.
  ASSIGN COMPONENT 'RCLNT'      OF STRUCTURE <w_externos> TO <rclnt>.
  ASSIGN COMPONENT 'SBXC_BLART_01'  OF STRUCTURE <w_externos> TO <sbxc_blart_01>.
  LOOP AT it_mov INTO wa_mov.
    l_tabix = sy-tabix.
    READ TABLE <t_externos> INTO <w_externos> INDEX 1.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    wa_mov-rsbxc_vbund_01_e = <rsbxc_vbund_01>.
    wa_mov-rsbxc_kunnr_01_e = <rsbxc_kunnr_01>.
    wa_mov-rsbxc_lifnr_01_e = <rsbxc_lifnr_01>.
    wa_mov-racct_e = <racct>.
    wa_mov-sbxc_xblnr_01_e = <sbxc_xblnr_01>.
    wa_mov-activ_e          = <activ>.
    wa_mov-awtyp_e          = <awtyp>.
    wa_mov-awkey_e          = <awkey>.
    wa_mov-aworg_e          = <aworg>.
    wa_mov-rbukrs_e         = <rbukrs>.
    wa_mov-refdocnr_e       = <refdocnr>.
    wa_mov-rclnt_e          = <rclnt>.
    wa_mov-description_e    = <sbxc_bktxt_01>.
    wa_mov-sbxc_blart_01_e  = <sbxc_blart_01>.

    PERFORM determine_fi_doc  USING <awtyp>
                                    <refdocnr>
                                    <aworg>
                                    <rbukrs>
                                    <rclnt>
                           CHANGING wa_mov-docnr_e .

    wa_mov-budat_e = <budat>.
    wa_mov-tsl_e = - <tsl>.
    wa_mov-hsl_e = - <hsl>.
    wa_mov-rtcur_e = <rtcur>.
    MODIFY it_mov FROM wa_mov INDEX l_tabix.
    DELETE <t_externos> INDEX 1.
  ENDLOOP.
  ASSIGN COMPONENT 'GL_SIRID'   OF STRUCTURE <w_externos> TO <gl_sirid>.
  ASSIGN COMPONENT 'SBXC_CONCI' OF STRUCTURE <w_externos> TO <sbxc_conci>.
  ASSIGN COMPONENT 'SBXC_ST_NX' OF STRUCTURE <w_externos> TO <sbxc_st_nx>.
  LOOP AT <t_externos> INTO <w_externos>.
    CHECK <sbxc_conci> = '' OR  <sbxc_st_nx> = 'X'.
    CLEAR: wa_mov_e.

    wa_mov_e-racct          = <racct>.
    wa_mov_e-description    = <sbxc_bktxt_01>.
    PERFORM determine_fi_doc  USING <awtyp>
                                    <refdocnr>
                                    <aworg>
                                    <rbukrs>
                                    <rclnt>
                           CHANGING wa_mov_e-docnr .
    wa_mov_e-budat = <budat>.
    wa_mov_e-tsl = - <tsl>.
    wa_mov_e-hsl = - <hsl>.
    wa_mov_e-rtcur = <rtcur>.
    wa_mov_e-sbxc_blart_01 = <sbxc_blart_01>.
    APPEND wa_mov_e TO it_mov_e.
    DELETE <t_externos>.
  ENDLOOP.

ENDFORM.                    "get_movs_s
*&---------------------------------------------------------------------*
*&      Form  GET_MOVS_F
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_movs_f .
  TYPES: BEGIN OF lty_saldos_conta,
           racct TYPE racct,
           hsl   TYPE /sbxc/conce096,
           tsl   TYPE /sbxc/conce096,
           rtcur TYPE rtcur,
         END OF lty_saldos_conta,
         lty_t_saldos_conta TYPE STANDARD TABLE OF lty_saldos_conta.

  DATA: t_conct002         TYPE TABLE OF /sbxc/conct002 WITH HEADER LINE,
*        t_t881             TYPE TABLE OF t881 WITH HEADER LINE,    "RSM-ID-18.05-del
*        t_dd03l            TYPE TABLE OF dd03l WITH HEADER LINE,   "RSM-ID-18.05-del
*        ncaract            TYPE i,                                 "RSM-ID-18.05-del
        t_fcttabint        TYPE lvc_t_fcat,
        t_fcttabext        TYPE lvc_t_fcat,
        w_fcttabint        TYPE lvc_s_fcat,
        w_fcttabext        TYPE lvc_s_fcat,
        dyn_internos       TYPE REF TO data,
        dyn_externos       TYPE REF TO data,
        dyn_winternos      TYPE REF TO data,
        dyn_wexternos      TYPE REF TO data,
        d_internos_lista   TYPE REF TO data,
        d_externos_lista   TYPE REF TO data,
        w_internos_lista   TYPE REF TO data,
        w_externos_lista   TYPE REF TO data,
        wa_saldos_i        TYPE /sbxc/concs035,
        wa_saldos_e        TYPE /sbxc/concs036,
        sbxc_perio         TYPE /sbxc/conce001,
        l_tabix            TYPE sy-tabix,
        l_subrc            TYPE sy-subrc,
        l_sort(20),
        l_action           TYPE /sbxc/conce099,
        lwa_saldos_conta_i TYPE lty_saldos_conta,
        lwa_saldos_conta_e TYPE lty_saldos_conta,
        lt_saldos_conta_i  TYPE lty_t_saldos_conta,
        lt_saldos_conta_e  TYPE lty_t_saldos_conta,
        lwa_skat           TYPE skat ##NEEDED,
        lwa_t001           TYPE t001.
*        lwa_conct033       TYPE /sbxc/conct033. "RSM-ID-18.11-del

  DATA: lit_saldos_i_f TYPE TABLE OF  /sbxc/concs035,
        lit_saldos_e_f TYPE TABLE OF  /sbxc/concs036.

  FIELD-SYMBOLS: <t_internos>       TYPE STANDARD TABLE,
                 <t_externos>       TYPE STANDARD TABLE,
                 <t_internos_lista> TYPE STANDARD TABLE,
                 <t_externos_lista> TYPE STANDARD TABLE,
                 <sbxc_conci>       TYPE any,
                 <sbxc_st_nx>       TYPE any,
                 <rldnr>            TYPE any,
                 <w_internos>       TYPE any,
                 <w_externos>       TYPE any,
                 <w_internos_lista> TYPE any,
                 <w_externos_lista> TYPE any,
                 <gl_sirid>         TYPE any,
                 <rsbxc_vbund_01>   TYPE any,
                 <rsbxc_lifnr_01>   TYPE any,
                 <rsbxc_kunnr_01>   TYPE any,
                 <sbxc_xblnr_01>    TYPE any,
                 <sbxc_bktxt_01>    TYPE any,
                 <rbukrs>           TYPE any,
                 <racct>            TYPE any,
                 <docnr>            TYPE any,
                 <budat>            TYPE any,
                 <tsl>              TYPE any,
                 <hsl>              TYPE any,
                 <rtcur>            TYPE any,
                 <refdocnr>         TYPE any,
                 <awtyp>            TYPE any,
                 <aworg>            TYPE any,
                 <activ>            TYPE any,
                 <rclnt>            TYPE any,
                 <sbxc_blart_01>    TYPE any.
* RSM-ID-18.05-ini
*  CLEAR:   t_conct002, t_t881, t_dd03l.
*  REFRESH: t_conct002, t_t881, t_dd03l.
*
*  SELECT * FROM /sbxc/conct002 CLIENT SPECIFIED INTO CORRESPONDING
*  FIELDS OF TABLE t_conct002 WHERE mandt = sy-mandt.
*  SELECT * FROM t881 INTO CORRESPONDING FIELDS OF TABLE t_t881 FOR ALL
*  ENTRIES IN t_conct002
*  WHERE rldnr  = t_conct002-sbxc_rldnri
*  OR rldnr = t_conct002-sbxc_rldnrfi.
*  LOOP AT t_t881.
*    ncaract = strlen( t_t881-tab ) - 1.
*    CONCATENATE t_t881-tab(ncaract) 'A' INTO t_t881-tab.
*    MODIFY t_t881.
*  ENDLOOP.
*  IF NOT t_t881[] IS INITIAL.
*    SELECT DISTINCT * FROM dd03l INTO CORRESPONDING FIELDS OF TABLE
*  t_dd03l FOR ALL ENTRIES IN t_t881
*    WHERE tabname = t_t881-tab.
*  ENDIF.
*  SORT t_dd03l BY position tabname.
*  REFRESH: t_fcttabint.
  DATA: lit_dd03l TYPE TABLE OF dd03l,
        lwa_dd03l TYPE dd03l.

  CLEAR: lit_dd03l[], lwa_dd03l, t_conct002, t_conct002[], t_fcttabint[].
  lit_dd03l = /sbxc/cl_conc_generic_cust=>get_all_ledger_fields(
               i_tp_internal = abap_true
               i_tp_external = abap_false  ).

* RSM-ID-18.05-fim
  PERFORM adicionar_elemento USING t_fcttabint '/SBXC/CONCS012' 'BOX'.
  PERFORM adicionar_elemento USING t_fcttabint '/SBXC/CONCS012' 'ROWCOLOR' .
  PERFORM adicionar_elemento USING t_fcttabint '/SBXC/CONCS012' 'BUTTON' .
  PERFORM adicionar_elemento USING t_fcttabint '/SBXC/CONCS012' 'SBXC_COMENT' .
  PERFORM adicionar_elemento USING t_fcttabint '/SBXC/CONCS012' 'SBXC_ST' .
  PERFORM adicionar_elemento USING t_fcttabint '/SBXC/CONCS012' 'SBXC_ST_NX' .
  PERFORM adicionar_elemento USING t_fcttabint '/SBXC/CONCS012' 'CELLSTYLE'.
  PERFORM adicionar_elemento USING t_fcttabint '/SBXC/CONCS012' 'SBXC_PERVAL'.
* RSM-ID-18.05-ini
*  LOOP AT t_dd03l WHERE rollname <> ''.
*    READ TABLE t_fcttabint INTO w_fcttabint WITH KEY fieldname = t_dd03l-fieldname.
*    IF sy-subrc NE 0.
*      PERFORM adicionar_elemento USING t_fcttabint t_dd03l-tabname t_dd03l-fieldname.
*    ENDIF.
*  ENDLOOP.
  LOOP AT lit_dd03l INTO lwa_dd03l WHERE rollname <> ''.
    READ TABLE t_fcttabint INTO w_fcttabint WITH KEY fieldname = lwa_dd03l-fieldname.
    IF sy-subrc NE 0.
      PERFORM adicionar_elemento USING t_fcttabint lwa_dd03l-tabname lwa_dd03l-fieldname.
    ENDIF.
  ENDLOOP.
* RSM-ID-18.05-fim
  PERFORM adicionar_estrutura USING '/SBXC/CONCS011' t_fcttabint.
  PERFORM adicionar_elemento USING t_fcttabint 'T882' 'CURR1'.
  PERFORM adicionar_elemento USING t_fcttabint 'T882' 'CURR2'.
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = t_fcttabint
    IMPORTING
      ep_table        = dyn_internos.
  ASSIGN dyn_internos->* TO <t_internos>.
  CREATE DATA dyn_winternos LIKE LINE OF <t_internos>.
  ASSIGN dyn_winternos->* TO <w_internos>.

* RSM-ID-18.05-ini
*  CLEAR:   t_conct002, t_t881, t_dd03l.
*  REFRESH: t_conct002, t_t881, t_dd03l.
*  SELECT * FROM /sbxc/conct002 CLIENT SPECIFIED INTO CORRESPONDING
*  FIELDS OF TABLE t_conct002  WHERE mandt = sy-mandt.
*  SELECT * FROM t881 INTO CORRESPONDING FIELDS OF TABLE t_t881 FOR ALL
*  ENTRIES IN t_conct002
*  WHERE rldnr  = t_conct002-sbxc_rldnre
*  OR rldnr  = t_conct002-sbxc_rldnrfe.
*  LOOP AT t_t881.
*    ncaract = strlen( t_t881-tab ) - 1.
*    CONCATENATE t_t881-tab(ncaract) 'A' INTO t_t881-tab.
*    MODIFY t_t881.
*  ENDLOOP.
*  IF NOT t_t881[] IS INITIAL.
*    SELECT DISTINCT * FROM dd03l INTO CORRESPONDING FIELDS OF TABLE
*  t_dd03l FOR ALL ENTRIES IN t_t881 WHERE tabname = t_t881-tab.
*  ENDIF.
*  SORT t_dd03l BY position tabname.
*  REFRESH: t_fcttabext.

  CLEAR: lit_dd03l[], lwa_dd03l, t_conct002, t_conct002[], t_fcttabext[].
  lit_dd03l = /sbxc/cl_conc_generic_cust=>get_all_ledger_fields(
               i_tp_internal = abap_false
               i_tp_external = abap_true  ).

* RSM-ID-18.05-fim
  PERFORM adicionar_elemento USING t_fcttabext '/SBXC/CONCS012' 'BOX'.
  PERFORM adicionar_elemento USING t_fcttabext '/SBXC/CONCS012' 'ROWCOLOR' .
  PERFORM adicionar_elemento USING t_fcttabext '/SBXC/CONCS012' 'BUTTON' .
  PERFORM adicionar_elemento USING t_fcttabext '/SBXC/CONCS012' 'SBXC_COMENT' .
  PERFORM adicionar_elemento USING t_fcttabext '/SBXC/CONCS012' 'SBXC_ST' .
  PERFORM adicionar_elemento USING t_fcttabext '/SBXC/CONCS012' 'SBXC_ST_NX' .
  PERFORM adicionar_elemento USING t_fcttabext '/SBXC/CONCS012' 'CELLSTYLE'.
  PERFORM adicionar_elemento USING t_fcttabext '/SBXC/CONCS012' 'SBXC_PERVAL'.
* RSM-ID-18.05-ini
*  LOOP AT t_dd03l WHERE rollname <> ''.
*    READ TABLE t_fcttabext INTO w_fcttabext WITH KEY fieldname = t_dd03l-fieldname.
*    IF sy-subrc NE 0.
*      PERFORM adicionar_elemento USING t_fcttabext t_dd03l-tabname t_dd03l-fieldname.
*    ENDIF.
*  ENDLOOP.
  LOOP AT lit_dd03l INTO lwa_dd03l WHERE rollname <> ''.
    READ TABLE t_fcttabext INTO w_fcttabext WITH KEY fieldname = lwa_dd03l-fieldname.
    IF sy-subrc NE 0.
      PERFORM adicionar_elemento USING t_fcttabext lwa_dd03l-tabname lwa_dd03l-fieldname.
    ENDIF.
  ENDLOOP.
* RSM-ID-18.05-fim
  PERFORM adicionar_estrutura USING '/SBXC/CONCS011' t_fcttabext.
  PERFORM adicionar_elemento USING t_fcttabext 'T882' 'CURR1'.
  PERFORM adicionar_elemento USING t_fcttabext 'T882' 'CURR2'.
*Gerar Tabela comunicação itens internos
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = t_fcttabext
    IMPORTING
      ep_table        = dyn_externos.
  ASSIGN dyn_externos->* TO <t_externos>.
  CREATE DATA dyn_wexternos LIKE LINE OF <t_externos>.
  ASSIGN dyn_wexternos->* TO <w_externos>.

  SELECT SINGLE sbxc_perio_01 FROM /sbxc/conct004
                              INTO sbxc_perio
                             WHERE bu_partner = p_partn.
  CREATE DATA d_internos_lista LIKE TABLE OF <w_internos>.
  CREATE DATA d_externos_lista LIKE TABLE OF <w_externos>.
  ASSIGN d_internos_lista->* TO <t_internos_lista>.
  ASSIGN d_externos_lista->* TO <t_externos_lista>.
  CREATE DATA w_internos_lista LIKE LINE OF <t_internos_lista>.
  CREATE DATA w_externos_lista LIKE LINE OF <t_externos_lista>.
  ASSIGN w_internos_lista->*   TO <w_internos_lista>.
  ASSIGN w_externos_lista->*   TO <w_externos_lista>.

  CLEAR: lit_saldos_i_f, lit_saldos_e_f.
  REFRESH: lit_saldos_i_f, lit_saldos_e_f,
           <t_internos>, <t_externos>,
           <t_internos_lista>, <t_externos_lista>.

  CALL FUNCTION '/SBXC/CONC_LERMOVIMENTOS'
    EXPORTING
      i_bukrs       = p_bukrs
      i_partner     = p_partn
      i_datumi      = tp_budati
      i_datumf      = tp_budatf
      sbxc_perio    = sbxc_perio
      i_fluxos      = 'X'
      l_pend        = '1'
      l_prm         = '1'
      l_pra         = '1'
      l_cnm         = '1'
      l_cna         = '1'
    TABLES
      t_itens_i     = <t_internos>
      t_itens_e     = <t_externos>
      t_itens_i_all = <t_internos_lista>
      t_itens_e_all = <t_externos_lista>.
  CALL FUNCTION '/SBXC/CONC_LERSALDO'
    EXPORTING
      i_bukrs    = p_bukrs
      i_partner  = p_partn
      i_datumi   = tp_budati
      i_datumf   = tp_budatf
      i_fluxos   = 'X'
      sbxc_perio = sbxc_perio
      l_pend     = '1'
      l_prm      = '0'
      l_pra      = '0'
      l_cnm      = '0'
      l_cna      = '0'
      l_cnt      = 'X'
      l_smd      = 'X'
    TABLES
      t_saldo_i  = lit_saldos_i_f
      t_saldo_e  = lit_saldos_e_f
      t_itens_i  = <t_internos_lista>
      t_itens_e  = <t_externos_lista>.

  ASSIGN COMPONENT 'RBUKRS' OF STRUCTURE <w_externos> TO <rbukrs>.
  LOOP AT <t_externos> INTO <w_externos>.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM t001 INTO CORRESPONDING FIELDS OF lwa_t001
    WHERE bukrs = <rbukrs>.
  ELSE.
*RSM-ID-18.11-ini
*    SELECT * FROM /sbxc/conct033 INTO lwa_conct033
*    WHERE bu_partner = p_partn.
*      SELECT SINGLE * FROM t001 INTO CORRESPONDING FIELDS OF lwa_t001
*      WHERE rcomp = lwa_conct033-rcomp.                     "#EC WARNOK
*      IF sy-subrc EQ 0.
*        EXIT.
*      ENDIF.
*    ENDSELECT.
    /sbxc/cl_conc_bp_cvi=>get_company_by_bp(
      EXPORTING
        i_tp_partner           = p_partn
      IMPORTING
        e_tp_company_code      = DATA(ltp_bukrs) ).

    SELECT SINGLE * FROM t001 INTO wa_t001_partner
    WHERE bukrs = ltp_bukrs.
*RSM-ID-18.11-fim
  ENDIF.

  LOOP AT lit_saldos_i_f INTO wa_saldos_i
  WHERE tipo = 3 AND racct NE ''. "RSM-20180628
    COLLECT wa_saldos_i INTO it_saldos_i_f.
  ENDLOOP.
  SORT it_saldos_i_f.
  LOOP AT lit_saldos_e_f INTO wa_saldos_e
  WHERE tipo = 3 AND racct NE ''. "RSM-20180628
    COLLECT wa_saldos_e INTO it_saldos_e_f.
  ENDLOOP.


* Regularização de IVA
  ASSIGN COMPONENT 'RACCT' OF STRUCTURE <w_internos> TO <racct>.
  ASSIGN COMPONENT 'RLDNR' OF STRUCTURE <w_internos> TO <rldnr>.
  ASSIGN COMPONENT 'TSL' OF STRUCTURE <w_internos> TO <tsl>.
  ASSIGN COMPONENT 'HSL' OF STRUCTURE <w_internos> TO <hsl>.
  ASSIGN COMPONENT 'RTCUR' OF STRUCTURE <w_internos> TO <rtcur>.

  LOOP AT <t_internos> INTO <w_internos>.
    CHECK <racct> = ''.
    CLEAR: wa_mov_i, wa_mov_e.
    IF <rldnr> = t_conct002-sbxc_rldnri OR <rldnr> = t_conct002-sbxc_rldnrfi.
      wa_mov_e-description    = co_descr_reg_iva.
      wa_mov_e-tsl = - <tsl>.
      wa_mov_e-hsl = - <hsl>.
      wa_mov_e-rtcur = <rtcur>.
      IF wa_mov_e-rtcur = wa_t001-waers.
        wa_mov_e-hsl = wa_mov_e-tsl.
      ENDIF.
      APPEND wa_mov_e TO it_mov_e_f.
    ELSE.
      wa_mov_i-description    = co_descr_reg_iva.
      wa_mov_i-tsl = - <tsl>.
      wa_mov_i-hsl = - <hsl>.
      wa_mov_i-rtcur = <rtcur>.
      IF wa_mov_i-rtcur = wa_t001-waers.
        wa_mov_i-hsl = wa_mov_i-tsl.
      ENDIF.
      APPEND wa_mov_i TO it_mov_i_f.
    ENDIF.
    DELETE <t_internos>.
  ENDLOOP.

  ASSIGN COMPONENT 'RACCT' OF STRUCTURE <w_externos> TO <racct>.
  ASSIGN COMPONENT 'RLDNR' OF STRUCTURE <w_internos> TO <rldnr>.
  ASSIGN COMPONENT 'TSL' OF STRUCTURE <w_externos> TO <tsl>.
  ASSIGN COMPONENT 'HSL' OF STRUCTURE <w_externos> TO <hsl>.
  ASSIGN COMPONENT 'RTCUR' OF STRUCTURE <w_externos> TO <rtcur>.
  LOOP AT <t_externos> INTO <w_externos>.
    CHECK <racct> = ''.
    CLEAR: wa_mov.
    IF <rldnr> = t_conct002-sbxc_rldnre OR <rldnr> = t_conct002-sbxc_rldnrfe.
      wa_mov_i-description    = co_descr_reg_iva.
      wa_mov_i-tsl = - <tsl>.
      wa_mov_i-hsl = - <hsl>.
      wa_mov_i-rtcur = <rtcur>.
      IF wa_mov_i-rtcur = wa_t001-waers.
        wa_mov_i-hsl = wa_mov_i-tsl.
      ENDIF.
      APPEND wa_mov_i TO it_mov_i_f.
    ELSE.
      wa_mov_e-description    = co_descr_reg_iva.
      wa_mov_e-tsl = - <tsl>.
      wa_mov_e-hsl = - <hsl>.
      wa_mov_e-rtcur = <rtcur>.
      IF wa_mov_e-rtcur = wa_t001-waers.
        wa_mov_e-hsl = wa_mov_e-tsl.
      ENDIF.
      APPEND wa_mov_e TO it_mov_e_f.
    ENDIF.
    DELETE <t_externos>.
  ENDLOOP.


*Tratar correções
  ASSIGN COMPONENT 'GL_SIRID'   OF STRUCTURE <w_internos> TO <gl_sirid>.
  ASSIGN COMPONENT 'SBXC_CONCI' OF STRUCTURE <w_internos> TO <sbxc_conci>.
  ASSIGN COMPONENT 'SBXC_ST_NX' OF STRUCTURE <w_internos> TO <sbxc_st_nx>.
  ASSIGN COMPONENT 'RLDNR' OF STRUCTURE <w_internos> TO <rldnr>.
  ASSIGN COMPONENT 'RACCT' OF STRUCTURE <w_internos> TO <racct>.
  READ TABLE t_conct002 WITH KEY sbxc_perio = sbxc_perio.
  CHECK sy-subrc EQ 0.
  LOOP AT <t_internos> INTO <w_internos>.
    l_tabix = sy-tabix.
    IF <sbxc_conci> = '' OR  <sbxc_st_nx> = 'X'.
      CHECK: <rldnr> = t_conct002-sbxc_rldnri OR <rldnr> = t_conct002-sbxc_rldnrfi.
      SELECT SINGLE action FROM /sbxc/conct036 INTO l_action
      WHERE gl_sirid = <gl_sirid> AND action = '1'.
      CHECK sy-subrc NE 0.
      MOVE-CORRESPONDING <w_internos> TO <w_externos>.
      APPEND <w_externos> TO <t_externos>.
      DELETE <t_internos> INDEX l_tabix.
    ELSE.
      DELETE <t_internos> INDEX l_tabix.
    ENDIF.
  ENDLOOP.
  ASSIGN COMPONENT 'GL_SIRID' OF STRUCTURE <w_externos> TO <gl_sirid>.
  ASSIGN COMPONENT 'SBXC_CONCI' OF STRUCTURE <w_externos> TO <sbxc_conci>.
  ASSIGN COMPONENT 'SBXC_ST_NX' OF STRUCTURE <w_externos> TO <sbxc_st_nx>.
  ASSIGN COMPONENT 'RLDNR' OF STRUCTURE <w_externos> TO <rldnr>.
  ASSIGN COMPONENT 'RACCT' OF STRUCTURE <w_externos> TO <racct>.
  LOOP AT <t_externos> INTO <w_externos>.
    l_tabix = sy-tabix.
    IF <sbxc_conci> = '' OR  <sbxc_st_nx> = 'X'.
      CHECK: <rldnr> = t_conct002-sbxc_rldnre OR <rldnr> = t_conct002-sbxc_rldnrfe.
      SELECT SINGLE action FROM /sbxc/conct036 INTO l_action
      WHERE gl_sirid = <gl_sirid> AND action = '1'.
      CHECK sy-subrc NE 0.
      MOVE-CORRESPONDING <w_externos> TO <w_internos>.
      APPEND <w_internos> TO <t_internos>.
      DELETE <t_externos> INDEX l_tabix.
    ELSE.
      DELETE <t_externos> INDEX l_tabix.
    ENDIF.
  ENDLOOP.

  ASSIGN COMPONENT 'RSBXC_VBUND_01' OF STRUCTURE <w_internos> TO  <rsbxc_vbund_01>.
  ASSIGN COMPONENT 'RSBXC_KUNNR_01' OF STRUCTURE <w_internos> TO  <rsbxc_kunnr_01>.
  ASSIGN COMPONENT 'RSBXC_LIFNR_01' OF STRUCTURE <w_internos> TO  <rsbxc_lifnr_01>.
  ASSIGN COMPONENT 'RACCT' OF STRUCTURE <w_internos> TO <racct>.
  ASSIGN COMPONENT 'SBXC_XBLNR_01' OF STRUCTURE <w_internos> TO  <sbxc_xblnr_01>.
  ASSIGN COMPONENT 'SBXC_BKTXT_01' OF STRUCTURE <w_internos> TO  <sbxc_bktxt_01>.
  ASSIGN COMPONENT 'DOCNR' OF STRUCTURE <w_internos> TO <docnr>.
  ASSIGN COMPONENT 'BUDAT' OF STRUCTURE <w_internos> TO <budat>.
  ASSIGN COMPONENT 'TSL' OF STRUCTURE <w_internos> TO <tsl>.
  ASSIGN COMPONENT 'HSL' OF STRUCTURE <w_internos> TO <hsl>.
  ASSIGN COMPONENT 'RTCUR' OF STRUCTURE <w_internos> TO <rtcur>.
  ASSIGN COMPONENT 'REFDOCNR' OF STRUCTURE <w_internos> TO <refdocnr>.
  ASSIGN COMPONENT 'AWTYP' OF STRUCTURE <w_internos> TO <awtyp>.
  ASSIGN COMPONENT 'AWORG' OF STRUCTURE <w_internos> TO <aworg>.
  ASSIGN COMPONENT 'GL_SIRID'   OF STRUCTURE <w_internos> TO <gl_sirid>.
  ASSIGN COMPONENT 'SBXC_CONCI' OF STRUCTURE <w_internos> TO <sbxc_conci>.
  ASSIGN COMPONENT 'SBXC_ST_NX' OF STRUCTURE <w_internos> TO <sbxc_st_nx>.
  ASSIGN COMPONENT 'ACTIV'      OF STRUCTURE <w_internos> TO <activ>.
  ASSIGN COMPONENT 'RBUKRS'     OF STRUCTURE <w_internos> TO <rbukrs>.
  ASSIGN COMPONENT 'RCLNT'      OF STRUCTURE <w_internos> TO <rclnt>.
  ASSIGN COMPONENT 'SBXC_BLART_01' OF STRUCTURE <w_internos> TO <sbxc_blart_01>.


  LOOP AT <t_internos> INTO <w_internos>.
    CHECK <sbxc_conci> = '' OR  <sbxc_st_nx> = 'X'.
    CHECK <racct> NE ''.
    CLEAR: wa_mov_i.

    PERFORM determine_fi_doc  USING <awtyp>
                                    <refdocnr>
                                    <aworg>
                                    <rbukrs>
                                    <rclnt>
                           CHANGING wa_mov_i-docnr .

    wa_mov_i-racct          = <racct>.
    wa_mov_i-description    = <sbxc_bktxt_01>.
    wa_mov_i-budat = <budat>.
    wa_mov_i-tsl = - <tsl>.
    wa_mov_i-hsl = - <hsl>.
    wa_mov_i-rtcur = <rtcur>.
    wa_mov_i-sbxc_blart_01 = <sbxc_blart_01>.

    APPEND wa_mov_i TO it_mov_i_f.
    DELETE <t_internos>.
  ENDLOOP.

  SORT it_mov_f BY budat_i.
  l_sort = 'BUDAT'.
  SORT <t_externos> BY (l_sort).
  ASSIGN COMPONENT 'RSBXC_VBUND_01' OF STRUCTURE <w_externos> TO  <rsbxc_vbund_01>.
  ASSIGN COMPONENT 'RSBXC_KUNNR_01' OF STRUCTURE <w_externos> TO  <rsbxc_kunnr_01>.
  ASSIGN COMPONENT 'RSBXC_LIFNR_01' OF STRUCTURE <w_externos> TO  <rsbxc_lifnr_01>.
  ASSIGN COMPONENT 'RACCT' OF STRUCTURE <w_externos> TO <racct>.
  ASSIGN COMPONENT 'SBXC_XBLNR_01' OF STRUCTURE <w_externos> TO  <sbxc_xblnr_01>.
  ASSIGN COMPONENT 'SBXC_BKTXT_01' OF STRUCTURE <w_externos> TO  <sbxc_bktxt_01>.
  ASSIGN COMPONENT 'DOCNR' OF STRUCTURE <w_externos> TO <docnr>.
  ASSIGN COMPONENT 'BUDAT' OF STRUCTURE <w_externos> TO <budat>.
  ASSIGN COMPONENT 'TSL' OF STRUCTURE <w_externos> TO <tsl>.
  ASSIGN COMPONENT 'HSL' OF STRUCTURE <w_externos> TO <hsl>.
  ASSIGN COMPONENT 'RTCUR' OF STRUCTURE <w_externos> TO <rtcur>.
  ASSIGN COMPONENT 'REFDOCNR' OF STRUCTURE <w_externos> TO <refdocnr>.
  ASSIGN COMPONENT 'AWTYP' OF STRUCTURE <w_externos> TO <awtyp>.
  ASSIGN COMPONENT 'AWORG' OF STRUCTURE <w_externos> TO <aworg>.
  ASSIGN COMPONENT 'GL_SIRID'   OF STRUCTURE <w_externos> TO <gl_sirid>.
  ASSIGN COMPONENT 'ACTIV'      OF STRUCTURE <w_externos> TO <activ>.
  ASSIGN COMPONENT 'RBUKRS'     OF STRUCTURE <w_externos> TO <rbukrs>.
  ASSIGN COMPONENT 'RCLNT'      OF STRUCTURE <w_externos> TO <rclnt>.
  ASSIGN COMPONENT 'SBXC_BLART_01' OF STRUCTURE <w_externos> TO <sbxc_blart_01>.

  LOOP AT it_mov_f INTO wa_mov.
    l_tabix = sy-tabix.
    READ TABLE <t_externos> INTO <w_externos> INDEX 1.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    wa_mov-rsbxc_vbund_01_e = <rsbxc_vbund_01>.
    wa_mov-rsbxc_kunnr_01_e = <rsbxc_kunnr_01>.
    wa_mov-rsbxc_lifnr_01_e = <rsbxc_lifnr_01>.
    wa_mov-racct_e = <racct>.
    wa_mov-sbxc_xblnr_01_e  = <sbxc_xblnr_01>.
    wa_mov-activ_e          = <activ>.
    wa_mov-awtyp_e          = <awtyp>.
    wa_mov-aworg_e          = <aworg>.
    wa_mov-rbukrs_e         = <rbukrs>.
    wa_mov-refdocnr_e       = <refdocnr>.
    wa_mov-rclnt_e          = <rclnt>.
    wa_mov-description_e    = <sbxc_bktxt_01>.
    wa_mov-sbxc_blart_01_e    = <sbxc_blart_01>.
    PERFORM determine_fi_doc  USING <awtyp>
                                    <refdocnr>
                                    <aworg>
                                    <rbukrs>
                                    <rclnt>
                           CHANGING wa_mov-docnr_e .

    wa_mov-budat_e = <budat>.
    wa_mov-tsl_e = - <tsl>.
    wa_mov-hsl_e = - <hsl>.
    wa_mov-rtcur_e = <rtcur>.
    MODIFY it_mov_f FROM wa_mov INDEX l_tabix.

    DELETE <t_externos> INDEX 1.
  ENDLOOP.
  ASSIGN COMPONENT 'GL_SIRID'   OF STRUCTURE <w_externos> TO <gl_sirid>.
  ASSIGN COMPONENT 'SBXC_CONCI' OF STRUCTURE <w_externos> TO <sbxc_conci>.
  ASSIGN COMPONENT 'SBXC_ST_NX' OF STRUCTURE <w_externos> TO <sbxc_st_nx>.
  ASSIGN COMPONENT 'REFDOCNR' OF STRUCTURE <w_externos> TO <refdocnr>.
  ASSIGN COMPONENT 'AWTYP' OF STRUCTURE <w_externos> TO <awtyp>.
  ASSIGN COMPONENT 'AWORG' OF STRUCTURE <w_externos> TO <aworg>.
  ASSIGN COMPONENT 'RBUKRS' OF STRUCTURE <w_externos> TO <rbukrs>.
  ASSIGN COMPONENT 'RCLNT' OF STRUCTURE <w_externos> TO <rclnt>.
  LOOP AT <t_externos> INTO <w_externos>.
    CHECK <sbxc_conci> = '' OR  <sbxc_st_nx> = 'X'.
    CHECK <racct> NE ''.
    CLEAR: wa_mov_e.

    PERFORM determine_fi_doc  USING <awtyp>
                                    <refdocnr>
                                    <aworg>
                                    <rbukrs>
                                    <rclnt>
                           CHANGING wa_mov_e-docnr .


    wa_mov_e-racct          = <racct>.
    wa_mov_e-description    = <sbxc_bktxt_01>.
    wa_mov_e-budat = <budat>.
    wa_mov_e-tsl = - <tsl>.
    wa_mov_e-hsl = - <hsl>.
    wa_mov_e-rtcur = <rtcur>.
    wa_mov_e-sbxc_blart_01 = <sbxc_blart_01>.

    APPEND wa_mov_e TO it_mov_e_f.
    DELETE <t_externos>.
  ENDLOOP.



ENDFORM.                    "get_movs_f
*&---------------------------------------------------------------------*
*&      Form  GET_CURRENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_currency .

**********************************************************************
* Visão interna
**********************************************************************
* Saldos
  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
    EXPORTING
      date             = tp_budatf
      foreign_amount   = '1'
      foreign_currency = wa_t001-waers
      local_currency   = wa_conct058-moeda_reporte
      type_of_rate     = wa_conct058-cat_taxa_camb_s
    IMPORTING
      exchange_rate    = tp_exchange_rate_is
    EXCEPTIONS
      no_rate_found    = 1
      overflow         = 2
      no_factors_found = 3
      no_spread_found  = 4
      derived_2_times  = 5
      OTHERS           = 6.
  IF sy-subrc <> 0.
    MESSAGE 'Erro ao det. taxa de câmbio (saldos)' TYPE 'E'.
  ENDIF.

  IF tp_exchange_rate_is LT 0.
    tp_exchange_rate_is = 1 / abs( tp_exchange_rate_is ).
  ENDIF.

* Fluxos
  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
    EXPORTING
      date             = tp_budatf
      foreign_amount   = '1'
      foreign_currency = wa_t001-waers
      local_currency   = wa_conct058-moeda_reporte
      type_of_rate     = wa_conct058-cat_taxa_camb_f
    IMPORTING
      exchange_rate    = tp_exchange_rate_if
    EXCEPTIONS
      no_rate_found    = 1
      overflow         = 2
      no_factors_found = 3
      no_spread_found  = 4
      derived_2_times  = 5
      OTHERS           = 6.
  IF sy-subrc <> 0.
    MESSAGE 'Erro ao det. taxa de câmbio (fluxos)' TYPE 'E'.
  ENDIF.

  IF tp_exchange_rate_if LT 0.
    tp_exchange_rate_if = 1 / abs( tp_exchange_rate_if ).
  ENDIF.

**********************************************************************
* Visão externa
**********************************************************************
  CHECK NOT wa_t001_partner-bukrs IS INITIAL.
* Saldos
  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
    EXPORTING
      date             = tp_budatf
      foreign_amount   = '1'
      foreign_currency = wa_t001_partner-waers
      local_currency   = wa_conct058-moeda_reporte
      type_of_rate     = wa_conct058-cat_taxa_camb_s
    IMPORTING
      exchange_rate    = tp_exchange_rate_es
    EXCEPTIONS
      no_rate_found    = 1
      overflow         = 2
      no_factors_found = 3
      no_spread_found  = 4
      derived_2_times  = 5
      OTHERS           = 6.
  IF sy-subrc <> 0.
    MESSAGE 'Erro ao det. taxa de câmbio (saldos)' TYPE 'E'.
  ENDIF.

  IF tp_exchange_rate_es LT 0.
    tp_exchange_rate_es = 1 / abs( tp_exchange_rate_es ).
  ENDIF.

* Fluxos
  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
    EXPORTING
      date             = tp_budatf
      foreign_amount   = '1'
      foreign_currency = wa_t001_partner-waers
      local_currency   = wa_conct058-moeda_reporte
      type_of_rate     = wa_conct058-cat_taxa_camb_f
    IMPORTING
      exchange_rate    = tp_exchange_rate_ef
    EXCEPTIONS
      no_rate_found    = 1
      overflow         = 2
      no_factors_found = 3
      no_spread_found  = 4
      derived_2_times  = 5
      OTHERS           = 6.
  IF sy-subrc <> 0.
    MESSAGE 'Erro ao det. taxa de câmbio (fluxos)' TYPE 'E'.
  ENDIF.

  IF tp_exchange_rate_ef LT 0.
    tp_exchange_rate_ef = 1 / abs( tp_exchange_rate_ef ).
  ENDIF.


ENDFORM.                    "get_currency
*&---------------------------------------------------------------------*
*&      Form  MERGE_MOVS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM merge_movs .
  TYPES: BEGIN OF lty_count_docs,
           docnr         TYPE belnr_d,
           budat         TYPE budat,
           sbxc_blart_01 TYPE blart,
           count_s       TYPE i,
           count_f       TYPE i,
         END OF lty_count_docs.
  DATA: lit_count_docs TYPE TABLE OF lty_count_docs,
        lwa_count_docs TYPE lty_count_docs,
        lwa_mov        TYPE ty_mov_i.

  LOOP AT it_mov_i INTO lwa_mov.
    lwa_count_docs-docnr = lwa_mov-docnr.
    lwa_count_docs-budat = lwa_mov-budat.
    lwa_count_docs-sbxc_blart_01 = lwa_mov-sbxc_blart_01.
    lwa_count_docs-count_s = 1.
    lwa_count_docs-count_f = 0.
    COLLECT lwa_count_docs INTO lit_count_docs.
  ENDLOOP.

  LOOP AT it_mov_i_f INTO lwa_mov.
    lwa_count_docs-docnr = lwa_mov-docnr.
    lwa_count_docs-budat = lwa_mov-budat.
    lwa_count_docs-sbxc_blart_01 = lwa_mov-sbxc_blart_01.
    lwa_count_docs-count_s = 0.
    lwa_count_docs-count_f = 1.
    COLLECT lwa_count_docs INTO lit_count_docs.
  ENDLOOP.

  LOOP AT lit_count_docs INTO lwa_count_docs.
*   Se tem mais linhas de fluxos do que saldos para um doc, acrescenta linha em branco para o doc
    IF lwa_count_docs-count_s LT lwa_count_docs-count_f.
      CLEAR lwa_mov.
      lwa_mov-docnr = lwa_count_docs-docnr.
      lwa_mov-budat = lwa_count_docs-budat.
      lwa_mov-sbxc_blart_01 = lwa_count_docs-sbxc_blart_01.
      APPEND lwa_mov TO it_mov_i.
    ENDIF.
  ENDLOOP.

**********************************************************************
  CLEAR: lit_count_docs[].
  LOOP AT it_mov_e INTO lwa_mov.
    lwa_count_docs-docnr = lwa_mov-docnr.
    lwa_count_docs-budat = lwa_mov-budat.
    lwa_count_docs-sbxc_blart_01 = lwa_mov-sbxc_blart_01.
    lwa_count_docs-count_s = 1.
    lwa_count_docs-count_f = 0.
    COLLECT lwa_count_docs INTO lit_count_docs.
  ENDLOOP.

  LOOP AT it_mov_e_f INTO lwa_mov.
    lwa_count_docs-docnr = lwa_mov-docnr.
    lwa_count_docs-budat = lwa_mov-budat.
    lwa_count_docs-sbxc_blart_01 = lwa_mov-sbxc_blart_01.
    lwa_count_docs-count_s = 0.
    lwa_count_docs-count_f = 1.
    COLLECT lwa_count_docs INTO lit_count_docs.
  ENDLOOP.

  LOOP AT lit_count_docs INTO lwa_count_docs.
*   Se tem mais linhas de fluxos do que saldos para um doc, acrescenta linha em branco para o doc
    IF lwa_count_docs-count_s LT lwa_count_docs-count_f.
      CLEAR lwa_mov.
      lwa_mov-docnr = lwa_count_docs-docnr.
      lwa_mov-budat = lwa_count_docs-budat.
      lwa_mov-sbxc_blart_01 = lwa_count_docs-sbxc_blart_01.
      APPEND lwa_mov TO it_mov_e.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "merge_movs
*&---------------------------------------------------------------------*
*&      Form  HANDLE_BALANCE_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM handle_balance_tab .
*  DATA: nlinhas      TYPE i,
**        linha_atual  TYPE i,
*        cod_emp(4)   TYPE c,    " Código da Empresa
*        desc_emp(25) TYPE c,  " Descrição da Empresa
*        dt_ref(10)   TYPE c,    " Data de Referencia
*        resp_emp(12) TYPE c   " Responsável
*        .
*
*  cod_emp = p_bukrs.
*  desc_emp = wa_t001-butxt.
*  dt_ref = tp_budatf.
*  resp_emp = sy-uname.
*  aux_tab_saldos = 0.
*
*  LOOP AT t_saldos_conta INTO wa_saldos_conta.
*
*    DATA: tmp_var(20)  TYPE c, tmp_var2(20) TYPE c ##NEEDED.
*    " Tratamento do texto referente a conta
*    SPLIT wa_saldos_conta-acc_e AT ' ' INTO tmp_var tmp_var2.
*    wa_saldos_conta-acc_e = tmp_var.
*    SPLIT wa_saldos_conta-acc_i AT ' ' INTO tmp_var tmp_var2.
*    wa_saldos_conta-acc_i = tmp_var.
*
*    IF sy-tabix EQ 1.
*      wa_saldos_conta-aux_info1 = 'Empresa Emissora:'.
*      wa_saldos_conta-aux_info2 = cod_emp.
*      wa_saldos_conta-aux_info3 = desc_emp.
*
*    ELSEIF sy-tabix EQ 2.
*      wa_saldos_conta-aux_info1 = 'Data Referência:'.
*      wa_saldos_conta-aux_info2 = dt_ref.
*
*    ELSEIF sy-tabix EQ 3.
*      wa_saldos_conta-aux_info1 = 'Responsável:'.
*      wa_saldos_conta-aux_info2 = resp_emp.
*    ENDIF.
*    " Atualizar os dados da tabela interna
*    MODIFY t_saldos_conta FROM wa_saldos_conta.
*
*  ENDLOOP.
*
*  DESCRIBE TABLE t_saldos_conta LINES nlinhas.
*
*
*  IF nlinhas EQ 0.
*    CLEAR wa_saldos_conta.
*    wa_saldos_conta-aux_info1 = 'Empresa Emissora:'.
*    wa_saldos_conta-aux_info2 = cod_emp.
*    wa_saldos_conta-aux_info3 = desc_emp.
*    APPEND wa_saldos_conta TO t_saldos_conta.
*
*    CLEAR wa_saldos_conta.
*    wa_saldos_conta-aux_info1 = 'Data Referência:'.
*    wa_saldos_conta-aux_info2 = dt_ref.
*    APPEND wa_saldos_conta TO t_saldos_conta.
*
*    CLEAR wa_saldos_conta.
*    wa_saldos_conta-aux_info1 = 'Responsável:'.
*    wa_saldos_conta-aux_info2 = resp_emp.
*    APPEND wa_saldos_conta TO t_saldos_conta.
*    aux_tab_saldos = aux_tab_saldos + 3.
*  ELSEIF nlinhas EQ 1.
*    CLEAR wa_saldos_conta.
*    wa_saldos_conta-aux_info1 = 'Data Referência:'.
*    wa_saldos_conta-aux_info2 = dt_ref.
*    APPEND wa_saldos_conta TO t_saldos_conta.
*
*    CLEAR wa_saldos_conta.
*    wa_saldos_conta-aux_info1 = 'Responsável:'.
*    wa_saldos_conta-aux_info2 = resp_emp.
*    APPEND wa_saldos_conta TO t_saldos_conta.
*
*    aux_tab_saldos = aux_tab_saldos + 2.
*  ELSEIF nlinhas EQ 2.
*
*    CLEAR wa_saldos_conta.
*    wa_saldos_conta-aux_info1 = 'Responsável:'.
*    wa_saldos_conta-aux_info2 = resp_emp.
*    APPEND wa_saldos_conta TO t_saldos_conta.
*
*    aux_tab_saldos = aux_tab_saldos + 1.
*
*  ENDIF.
*ENDFORM.                    "handle_balance_tab

*&---------------------------------------------------------------------*
*&      Form  adicionar_elemento
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_FCTTAB text
*      -->P_TABNAME  text
*      -->P_FILDNAME text
*----------------------------------------------------------------------*
FORM adicionar_elemento  USING  p_t_fcttab TYPE lvc_t_fcat
                                p_tabname p_fildname.
  DATA: datatype    TYPE dd03l-datatype,
        w_fcttabint TYPE lvc_s_fcat.
  w_fcttabint-coltext = ' '.
  w_fcttabint-seltext = ' '.
  w_fcttabint-scrtext_l = ' '.
  w_fcttabint-scrtext_m = ' '.
  w_fcttabint-scrtext_s = ' '.
  w_fcttabint-fieldname = p_fildname.
  w_fcttabint-ref_table = p_tabname.
  w_fcttabint-ref_field = p_fildname.
  PERFORM tamanho_campo USING p_tabname
                              p_fildname
                    CHANGING  w_fcttabint-intlen w_fcttabint-convexit w_fcttabint-cfieldname datatype.
  APPEND w_fcttabint TO p_t_fcttab.
ENDFORM.                    "adicionar_elemento
*&---------------------------------------------------------------------*
*&      Form  tamanho_campo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TABNAME    text
*      -->FIELDNAME  text
*      -->LENGTH     text
*      -->CONVEXIT   text
*      -->CFIELDNAME text
*      -->DATATYPE   text
*----------------------------------------------------------------------*
FORM tamanho_campo  USING  tabname
                           fieldname
                  CHANGING length convexit cfieldname datatype.
  DATA domname TYPE dd01l-domname.
  SELECT SINGLE domname reffield datatype FROM dd03l INTO (domname, cfieldname, datatype)
  WHERE tabname = tabname
  AND  fieldname = fieldname.                               "#EC WARNOK
  IF sy-subrc = 0.
    SELECT SINGLE outputlen convexit FROM dd01l INTO (length, convexit) WHERE domname = domname. "#EC WARNOK
  ENDIF.
ENDFORM.                    "tamanho_campo
*&---------------------------------------------------------------------*
*&      Form  adicionar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TABELA   text
*      -->P_T_FCTTAB text
*----------------------------------------------------------------------*
FORM adicionar_estrutura  USING    p_tabela
                                   p_t_fcttab TYPE lvc_t_fcat.
  DATA : it_tabdescr     TYPE abap_compdescr_tab,
         wa_tabdescr     TYPE abap_compdescr,
         ref_table_descr TYPE REF TO cl_abap_structdescr,
         datatype        TYPE dd03l-datatype,
         w_fcttabext     TYPE lvc_s_fcat.
  REFRESH: it_tabdescr.
  ref_table_descr ?= cl_abap_typedescr=>describe_by_name(  p_tabela ).
  it_tabdescr[] = ref_table_descr->components[].
  LOOP AT it_tabdescr INTO wa_tabdescr.
    CLEAR w_fcttabext.
    w_fcttabext-fieldname = wa_tabdescr-name.
    w_fcttabext-datatype  = wa_tabdescr-type_kind.
    w_fcttabext-inttype   = wa_tabdescr-type_kind.
    w_fcttabext-intlen    = wa_tabdescr-length.
    w_fcttabext-decimals  = wa_tabdescr-decimals.
    PERFORM tamanho_campo USING p_tabela
                                wa_tabdescr-name
                      CHANGING  w_fcttabext-intlen w_fcttabext-convexit w_fcttabext-cfieldname datatype.
    PERFORM texto_campo USING  p_tabela
                              w_fcttabext-fieldname
                        CHANGING w_fcttabext-coltext
                           w_fcttabext-seltext
                           w_fcttabext-scrtext_l
                           w_fcttabext-scrtext_m
                           w_fcttabext-scrtext_s.

    APPEND w_fcttabext TO p_t_fcttab.
  ENDLOOP.
ENDFORM.                    " ADICIONAR_ESTRUTURA
*&---------------------------------------------------------------------*
*&      Form  texto_campo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TABNAME    text
*      -->FIELDNAME  text
*      -->COLTEXT    text
*      -->SELTEXT    text
*      -->SCRTEXT_L  text
*      -->SCRTEXT_M  text
*      -->SCRTEXT_S  text
*----------------------------------------------------------------------*
FORM texto_campo  USING    tabname
                           fieldname
                  CHANGING coltext ##NEEDED
                           seltext ##NEEDED
                           scrtext_l
                           scrtext_m
                           scrtext_s.
  SELECT SINGLE scrtext_s scrtext_m scrtext_l
          FROM dd03m
          INTO (scrtext_s, scrtext_m, scrtext_l)
         WHERE tabname    = tabname
           AND fieldname  = fieldname
           AND ddlanguage = sy-langu.                       "#EC WARNOK
ENDFORM.                    "texto_campo
*RSM-ID-18.02-ini
**&---------------------------------------------------------------------*
**&      Form  CONVERT_TO_LOCAL_CURRENCY
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->L_RTCUR    text
**      -->L_WAERS    text
**      -->L_BUDAT    text
**      -->L_TSL      text
**      -->L_HSL      text
**----------------------------------------------------------------------*
*FORM convert_to_local_currency
*USING l_rtcur l_waers l_budat l_tsl
*      l_hsl.
*
*  IF l_rtcur <> l_waers AND NOT l_rtcur IS INITIAL.
*    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
*      EXPORTING
**       CLIENT           = SY-MANDT
*        date             = l_budat
*        foreign_amount   = l_tsl
*        foreign_currency = l_rtcur
*        local_currency   = l_waers
**       RATE             = 0
*        type_of_rate     = 'EURX'
**       READ_TCURR       = 'X'
*      IMPORTING
**       EXCHANGE_RATE    =
**       FOREIGN_FACTOR   =
*        local_amount     = l_hsl
**       LOCAL_FACTOR     =
**       EXCHANGE_RATEX   =
**       FIXED_RATE       =
**       DERIVED_RATE_TYPE =
*      EXCEPTIONS
*        no_rate_found    = 1
*        overflow         = 2
*        no_factors_found = 3
*        no_spread_found  = 4
*        derived_2_times  = 5
*        OTHERS           = 6.
*    IF sy-subrc <> 0.
*      CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
*        EXPORTING
**         CLIENT           = SY-MANDT
*          date             = l_budat
*          foreign_amount   = l_tsl
*          foreign_currency = l_rtcur
*          local_currency   = l_waers
**         RATE             = 0
*          type_of_rate     = 'M'
**         READ_TCURR       = 'X'
*        IMPORTING
**         EXCHANGE_RATE    =
**         FOREIGN_FACTOR   =
*          local_amount     = l_hsl
**         LOCAL_FACTOR     =
**         EXCHANGE_RATEX   =
**         FIXED_RATE       =
**         DERIVED_RATE_TYPE =
*        EXCEPTIONS
*          no_rate_found    = 1
*          overflow         = 2
*          no_factors_found = 3
*          no_spread_found  = 4
*          derived_2_times  = 5
*          OTHERS           = 6.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    "CONVERT_TO_LOCAL_CURRENCY
*RSM-ID-18.02-fim
*&---------------------------------------------------------------------*
*&      Form  GET_REPORT_SETTINGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_report_settings .
* Agrupadores para saldos
  SELECT * FROM /sbxc/conct055 INTO TABLE it_conct055_s WHERE saldos_fluxos = 'S'.
  SELECT * FROM /sbxc/conct055 INTO TABLE it_conct055_f WHERE saldos_fluxos = 'F'.

  SORT it_conct055_s BY agrupador.
  SORT it_conct055_f BY agrupador.

* Classificação de movimentos
  SELECT * FROM /sbxc/conct056 INTO TABLE it_conct056.  "#EC CI_NOWHERE

  SORT it_conct056 BY sequence.

* Ler parâmetros do report
  SELECT SINGLE * FROM /sbxc/conct058 INTO wa_conct058 WHERE id_reporte = p_idrep.

ENDFORM.                    "get_report_settings
*&---------------------------------------------------------------------*
*&      Form  SET_ROW_HEIGHT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_row_height .
  DATA: ltp_message TYPE string.
  TRY.
*     Obter folha de cálculo ativa
      o_worksheet = o_excel->get_active_worksheet( ).

      DO tp_row TIMES.
        o_worksheet->set_row_height( ip_row = sy-index ip_height_fix = '12.75' ).
      ENDDO.

    CATCH zcx_excel INTO x_excel.
      ltp_message = x_excel->get_text( ).
      MESSAGE ltp_message TYPE 'E'.
  ENDTRY.
ENDFORM.                    " SET_ROW_HEIGHT

*&---------------------------------------------------------------------*
*&      Form  determine_fi_doc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_AWTYP    text
*      -->P_REFDOCNR text
*      -->P_AWORG    text
*      -->P_RBUKRS   text
*      -->P_RCLNT    text
*      <--C_BELNR    text
*----------------------------------------------------------------------*
FORM determine_fi_doc  USING  p_awtyp
                              p_refdocnr
                              p_aworg
                              p_rbukrs
                              p_rclnt
                     CHANGING c_belnr.
  DATA: lit_bkpf    TYPE TABLE OF bkpf,
        lwa_bkpf    TYPE bkpf,

        ltp_destino TYPE rfcdest.

  CLEAR ltp_destino.

  SELECT SINGLE rfcdest FROM /sbxc/conct043 INTO ltp_destino WHERE sbxc_mandt = p_rclnt.
  IF sy-subrc NE 0 OR ltp_destino IS INITIAL.
    ltp_destino = 'NONE'.
  ENDIF.

  CALL FUNCTION 'FI_DOCUMENT_READ' "#EC CI_USAGE_OK[2438110]
    DESTINATION ltp_destino
    EXPORTING
      i_awtyp                    = p_awtyp
      i_awref                    = p_refdocnr
      i_aworg                    = p_aworg
      i_bukrs                    = p_rbukrs
      i_check_for_mapping_issues = abap_false "RSM-ID-18.03-ins
    TABLES
      t_bkpf                     = lit_bkpf
    EXCEPTIONS
      wrong_input                = 1
      not_found                  = 2
      OTHERS                     = 3.                                      "#EC ENHOK
  IF sy-subrc EQ 0.
    READ TABLE lit_bkpf INTO lwa_bkpf INDEX 1.
    IF sy-subrc EQ 0.
      c_belnr = lwa_bkpf-belnr.
    ENDIF.
  ENDIF.

ENDFORM.                    "determine_fi_doc
*&---------------------------------------------------------------------*
*&      Form  selection_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--ERRO       text
*----------------------------------------------------------------------*
FORM selection_screen CHANGING erro.

  PERFORM determinar_periodo USING  p_perio CHANGING f_name erro.
  LOOP AT SCREEN.
    IF NOT screen-group1 IS INITIAL.
      IF screen-group1 = f_name(3) OR ( f_name <> 'DATUM'
                                   AND screen-group1 = 'GJA' )
                                   AND NOT p_perio IS INITIAL
                                   AND NOT f_name  IS INITIAL.
        screen-input      = '1'.
        screen-output     = '1'.
        screen-invisible  = '0'.
        screen-required   = '1'.
      ELSEIF NOT screen-group1 IS INITIAL.
        IF screen-group1 EQ 'GJA' OR screen-group1 EQ 'MON' OR screen-group1 EQ 'KWE' OR screen-group1 EQ 'TRI' OR screen-group1 EQ 'SEM' OR screen-group1 EQ 'DAT'.
          screen-input      = '0'.
          screen-output     = '0'.
          screen-invisible  = '1'.
          screen-required   = '0'.
        ENDIF.
      ENDIF.

      IF screen-group1 EQ 'FNT'.
        IF r_down EQ 'X'.
          screen-input      = '1'.
          screen-output     = '1'.
          screen-invisible  = '0'.
          screen-required   = '0'.
        ELSE.
          screen-input      = '0'.
          screen-output     = '0'.
          screen-invisible  = '1'.
          screen-required   = '0'.
        ENDIF.
      ENDIF.
      IF screen-group1 EQ 'BCK'.
        IF r_back EQ 'X'.
          screen-input      = '1'.
          screen-output     = '1'.
          screen-invisible  = '0'.
          screen-required   = '0'.
        ELSE.
          screen-input      = '0'.
          screen-output     = '0'.
          screen-invisible  = '1'.
          screen-required   = '0'.
        ENDIF.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "selection_screen

*&---------------------------------------------------------------------*
*&      Form  determinar_periodo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PERIODO    text
*      <--FIELD_NAME text
*      <--ERRO       text
*----------------------------------------------------------------------*
FORM determinar_periodo USING periodo CHANGING field_name erro.
  CLEAR field_name.
  IF NOT periodo IS INITIAL.
    SELECT SINGLE sbxc_field FROM /sbxc/conct010
                             INTO field_name
                            WHERE sbxc_perio = periodo.
    erro = sy-subrc.
  ENDIF.
ENDFORM.                    "determinar_periodo

*&---------------------------------------------------------------------*
*&      Form  determina_datas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--DATUM_I    text
*      <--DATUM_F    text
*----------------------------------------------------------------------*
FORM determina_datas CHANGING datum_i datum_f.
  DATA: week LIKE  scal-week,
        date LIKE  scal-date.
  CASE f_name.
    WHEN: 'GJAHR'.
      CONCATENATE p_gjahr '01' '01'
             INTO datum_i .
      CONCATENATE p_gjahr '12' '31'
             INTO datum_f.
    WHEN: 'MONAT'.
      CONCATENATE p_gjahr p_monat '01'
             INTO datum_i.
      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = datum_i
        IMPORTING
          last_day_of_month = datum_f
        EXCEPTIONS
          day_in_no_date    = 1
          OTHERS            = 2.
    WHEN: 'DATUM'.
      datum_i = p_datum.
      datum_f = p_datum.
    WHEN: 'KWEEK'.
      CONCATENATE p_gjahr p_kweek
             INTO week.
      CALL FUNCTION 'WEEK_GET_FIRST_DAY'
        EXPORTING
          week         = week
        IMPORTING
          date         = date
        EXCEPTIONS
          week_invalid = 1
          OTHERS       = 2.
      datum_i = date.
      datum_f = date + 6.
    WHEN: 'TRIMS'.
      IF p_trims = '1'.
        CONCATENATE p_gjahr '01' '01'
             INTO datum_i.
        CONCATENATE p_gjahr '03' '31'
               INTO datum_f.
      ELSEIF p_trims = '2'.
        CONCATENATE p_gjahr '04' '01'
             INTO datum_i.
        CONCATENATE p_gjahr '06' '31'
               INTO datum_f.
      ELSEIF p_trims = '3'.
        CONCATENATE p_gjahr '07' '01'
             INTO datum_i.
        CONCATENATE p_gjahr '09' '30'
               INTO datum_f.
      ELSEIF p_trims = '4'.
        CONCATENATE p_gjahr '10' '01'
             INTO datum_i.
        CONCATENATE p_gjahr '12' '31'
               INTO datum_f.
      ENDIF.
    WHEN: 'SEMES'.
      IF p_semes = '1'.
        CONCATENATE p_gjahr '01' '01'
             INTO datum_i.
        CONCATENATE p_gjahr '06' '31'
               INTO datum_f.
      ELSEIF p_semes = '2'.
        CONCATENATE p_gjahr '07' '01'
             INTO datum_i.
        CONCATENATE p_gjahr '12' '31'
               INTO datum_f.
      ENDIF.
  ENDCASE.
ENDFORM.                    " DETERMINA_DATAS
