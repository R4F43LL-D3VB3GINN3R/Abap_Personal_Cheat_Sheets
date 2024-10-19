*&---------------------------------------------------------------------*
*& Report ZPDF_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPDF_TEST.

data: it_table type table of sflight.

select * from sflight into table it_table.

data:g_val         type c,
     w_pripar      type pri_params,
     w_arcpar      type arc_params,
     i_pdf         type table of tline,
     spoolid       type tsp01-rqident,
     l_no_of_bytes type i,
     l_pdf_spoolid type tsp01-rqident,
     l_jobname     type tbtcjob-jobname,
     l_jobcount    type tbtcjob-jobcount.
call function 'GET_PRINT_PARAMETERS'
  exporting
    in_archive_parameters  = w_arcpar
    in_parameters          = w_pripar
    layout                 = 'X_65_132'
    line_count             = 65
    line_size              = 132
    no_dialog              = 'X'
  importing
    out_archive_parameters = w_arcpar
    out_parameters         = w_pripar
    valid                  = g_val.
if g_val ne space and sy-subrc = 0.
  w_pripar-prrel = space.
  w_pripar-primm = space.
  new-page print on new-section parameters w_pripar archive parameters w_arcpar no dialog.
endif.
try.
    cl_salv_table=>factory(
    exporting
    list_display = if_salv_c_bool_sap=>false
    importing
    r_salv_table = data(it_final_display)
    changing
    t_table = it_table "Your internal table where data is available
    ).
  catch cx_salv_msg.
endtry.

it_final_display->display( ).
new-page print off.
call function 'ABAP4_COMMIT_WORK'.
spoolid = sy-spono.
call function 'CONVERT_ABAPSPOOLJOB_2_PDF'
  exporting
    src_spoolid   = spoolid
    no_dialog     = ' '
  importing
    pdf_bytecount = l_no_of_bytes
    pdf_spoolid   = l_pdf_spoolid
    btc_jobname   = l_jobname
    btc_jobcount  = l_jobcount
  tables
    pdf           = i_pdf.

call function 'GUI_DOWNLOAD'
  exporting
    filename = 'C:\Users\RafaelAlbuquerque\Desktop\Rafael\docpdf.pdf' "Give the pdf location of your system where you want to store
    filetype = 'BIN'
  tables
    data_tab = i_pdf.
