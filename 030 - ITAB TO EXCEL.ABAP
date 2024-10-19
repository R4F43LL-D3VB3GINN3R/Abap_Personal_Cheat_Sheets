report zrla_test_excel.

class lca_excel_pro definition.

  public section.

    data: it_data type table of sflight. "tabela interna
    data: o_xl type ref to zcl_excel.    "classe para manipulacao de excel

    methods:
      convert_xstring,
      download_xls
        importing
          filename type string.

  protected section.

  private section.

    methods:
      append_extension
        importing old_extension type string
        exporting new_extension type string,
      get_data,
      get_file_directory
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

    namefile = filename. " Nome base do arquivo

    " Adiciona a extensão '.xlsx' ao nome do arquivo
    me->append_extension(
      exporting
        old_extension = namefile
      importing
        new_extension = namefile
    ).

    " Diálogo para selecionar diretório e nome do arquivo
    call method cl_gui_frontend_services=>file_save_dialog
      exporting
        default_extension = 'xlsx'
        default_file_name = namefile
      changing
        filename          = namefile   " Nome final do arquivo escolhido
        path              = directory  " Diretório escolhido
        fullpath          = fullpath   " Caminho completo
      exceptions
        others            = 1.

    " Se o usuário não cancelar a operação
    if sy-subrc = 0.
      concatenate directory namefile into fullpath separated by '\'. " Cria o caminho completo do arquivo
    else.
      clear fullpath. " Caso o usuário cancele a operação
    endif.

    " Retorna o caminho completo do arquivo
    full_path = fullpath.

  endmethod.

  method append_extension.

    concatenate old_extension 'xlsx' into new_extension separated by '.'.

  endmethod.

  method convert_xstring.

    data(o_converter) = new zcl_excel_converter( ).

    " Consulta a base de dados
    me->get_data( ).

    " Converte os dados para o formato Excel
    o_converter->convert(
      exporting
        it_table      = me->it_data
      changing
        co_excel      = me->o_xl
    ).

    " Tratamento de erros
    if sy-subrc ne 0.
      message 'Não foi possível converter os dados para xstring' type 'S' display like 'E'.
      return.
    endif.

  endmethod.

  method download_xls.

    "----------------------------------------------------------------

    " Tratamento de nome e extensão do arquivo
    data full_path type string.
    data namefile type string.

    namefile = filename.

    " Seleciona o diretório e o nome do arquivo a ser salvo
    me->get_file_directory(
      exporting
        filename  = namefile
      importing
        full_path = full_path
    ).

    if full_path is initial.
      " Caso o usuário cancele a seleção, interrompe o processo
      message 'O download foi cancelado pelo usuário.' type 'S' display like 'E'.
      return.
    endif.

    "----------------------------------------------------------------

    " Conversão para xstring
    me->convert_xstring( ).

    "----------------------------------------------------------------

    " Download do arquivo
    data(o_xl_ws) = o_xl->get_active_worksheet( ).
    o_xl_ws->freeze_panes( ip_num_rows = 1 ).

    data(o_xlwriter) = cast zif_excel_writer( new zcl_excel_writer_2007( ) ).
    data(lv_xl_xdata) = o_xlwriter->write_file( o_xl ).
    data(it_raw_data) = cl_bcs_convert=>xstring_to_solix( exporting iv_xstring = lv_xl_xdata ).

    " Método para realizar o download do arquivo xls
    cl_gui_frontend_services=>gui_download(
      exporting
        filename    = full_path   " Caminho completo do arquivo
        filetype    = 'BIN'
        bin_filesize = xstrlen( lv_xl_xdata )
      changing
        data_tab    = it_raw_data
    ).

    "----------------------------------------------------------------

    " Tratamento de erros
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

  " Inicia o processo de download
  ol_xls->download_xls( filename = 'novo_arquivo' ).
