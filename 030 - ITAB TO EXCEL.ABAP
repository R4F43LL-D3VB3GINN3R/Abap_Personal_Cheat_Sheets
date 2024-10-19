report zrla_test_excel.

class lca_excel_pro definition.

  public section.

    data: it_data type table of sflight. "tabela interna
    data: o_xl type ref to zcl_excel.    "classe para manipulacao de excel

    methods:
      convert_xstring, "converte tabela interna para xstring
      download_xls.     "realiza download do arquivo excel
*        importing
*          filename type string.

  protected section.

  private section.

    methods:
      append_extension                       "prepara caminho para o arquivo com extensao
        importing old_extension type string
        exporting new_extension type string,
      get_data,                              "consulta sql
      get_file_directory                     "prepara o caminho para o arquivo
        importing
          filename type string
        exporting
          full_path type string.

endclass.

class lca_excel_pro implementation.

  method get_file_directory.

    data: namefile  type string,
          directory type string,
          fullpath  type string.

    namefile = 'file'.

    "adiciona a extensão '.xlsx' ao nome do arquivo
    me->append_extension(
      exporting
        old_extension = namefile
      importing
        new_extension = namefile
    ).

    "diálogo para selecionar diretorio e nome do arquivo
    call method cl_gui_frontend_services=>file_save_dialog
      exporting
        default_extension = 'xlsx'
        default_file_name = namefile
      changing
        filename          = namefile
        path              = directory
        fullpath          = fullpath
      exceptions
        others            = 1.

    "se o user nao cancelar a operacao...
    if sy-subrc = 0.
      concatenate directory namefile into fullpath separated by '\'. "cria diretorio completo do arquivo
    else.
      clear fullpath. "limpa o caminho
    endif.

    full_path = fullpath. "retorna caminho completo do arquivo

  endmethod.

  method append_extension.

    concatenate old_extension 'xlsx' into new_extension separated by '.'.

  endmethod.

  method convert_xstring.

    data(o_converter) = new zcl_excel_converter( ).

    "consulta a base de dados
    me->get_data( ).

    " Converte os dados para o formato Excel
    o_converter->convert(
      exporting
        it_table      = me->it_data
      changing
        co_excel      = me->o_xl
    ).

    "tratamento de erros
    if sy-subrc ne 0.
      message 'Não foi possível converter os dados para xstring' type 'S' display like 'E'.
      return.
    endif.

  endmethod.

  method download_xls.

    "----------------------------------------------------------------

    " tratamento de nome e extensão do arquivo
    data full_path type string.
    data namefile type string.

    namefile = 'file'.

    "metodo que salva nome e diretorio
    me->get_file_directory(
      exporting
        filename  = namefile
      importing
        full_path = full_path
    ).

    "se o download for cancelado...
    if full_path is initial.
      message 'O download foi cancelado pelo usuário.' type 'S' display like 'E'.
      return.
    endif.

    "----------------------------------------------------------------

    "converte dados para xstring
    me->convert_xstring( ).

    "----------------------------------------------------------------

    "download do arquivo
    data(o_xl_ws) = o_xl->get_active_worksheet( ).
    o_xl_ws->freeze_panes( ip_num_rows = 1 ).

    data(o_xlwriter) = cast zif_excel_writer( new zcl_excel_writer_2007( ) ).
    data(lv_xl_xdata) = o_xlwriter->write_file( o_xl ).
    data(it_raw_data) = cl_bcs_convert=>xstring_to_solix( exporting iv_xstring = lv_xl_xdata ).

    "download do arquivo xls
    cl_gui_frontend_services=>gui_download(
      exporting
        filename    = full_path
        filetype    = 'BIN'
        bin_filesize = xstrlen( lv_xl_xdata )
      changing
        data_tab    = it_raw_data
    ).

    "----------------------------------------------------------------

    "tratamento de erros.
    if sy-subrc ne 0.
      message 'Não foi possível realizar o download do arquivo' type 'S' display like 'E'.
      return.
    endif.

  endmethod.

  method get_data.

    select * from sflight into table me->it_data.

  endmethod.

endclass.

start-of-selection.

  data: ol_xls type ref to lca_excel_pro.

  create object ol_xls.

  "download
  ol_xls->download_xls(  ).
