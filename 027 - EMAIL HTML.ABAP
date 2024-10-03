*&---------------------------------------------------------------------*
*& Report ZHTML_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zhtml_test.

parameters: p_email type ad_smtpadr default 'RAFAEL.ALBUQUERQUE@SBX.COM'. "parametro de endereço de email

constants: lc_priority    type bcs_docimp value '1',
           lc_template_id type smtg_tmpl_id value 'ZRLA_TEMPLATE_EMAIL'.

types: begin of wa_pct,
         id_paciente   type zpacientes-id_paciente,
         nome_paciente type zpacientes-nome_paciente,
       end of wa_pct.

data: it_paciente type table of wa_pct,
      ls_paciente type wa_pct.

start-of-selection.

  select id_paciente,
         nome_paciente
    from zpacientes
    into table @it_paciente.

  if sy-subrc eq 0.

    data(lo_email_api_ref)    = cl_smtg_email_api=>get_instance( iv_template_id = lc_template_id ).
    data(lo_send_request_ref) = cl_bcs=>create_persistent( ).
    data(i_cds_key)           = value if_smtg_email_template=>ty_gt_data_key( ).

    lo_email_api_ref->render( exporting
                                iv_language  = sy-langu
                                it_data_key  = i_cds_key
                              importing
                                ev_subject   = data(lv_subject)
                                ev_body_html = data(lv_body_html) ).

  "--------------------------------------------------------------------------------------------
  "PENSAMENTO LÓGICO PRINCIPAL

    data: stringtable type table of string.

    "tabela recebe o texto html
    call function 'GUI_UPLOAD'
      exporting
        filename = 'C:\Users\RafaelAlbuquerque\Desktop\Rafael\htmltext.txt'
      tables
        data_tab = stringtable.

    data: id_pct type string. "casting

    data: htmltext type string. "recebe o html do template

    "passa o html para a string
    loop at stringtable into data(ls_data).
*      shift ls_data left deleting leading space.
*      shift ls_data right deleting trailing space.
      concatenate htmltext ls_data into htmltext.
    endloop.

    data: main_part type string, "parte principal do html
          row_part  type string, "linha da tabela
          end_part  type string. "parte final

    "funcao para separar o html em partes
    call function 'ZSEPARATE_HTML'
      exporting
        html_text       = htmltext
      IMPORTING
        MAIN            = main_part
        ROW             = row_part
        END             = end_part.

    clear htmltext. "limpa a variavel

    sort it_paciente by id_paciente. "organiza a tabela por id

    "itera sobre a tabela interna
    loop at it_paciente into ls_paciente.
      if '#' co row_part.                                                        "se for encontrada uma linha disponivel
        id_pct = ls_paciente-id_paciente.                                        "casting do id para string
        replace '#idpaciente#' with id_pct into main_part.                       "substitui valor
        replace '#nomepaciente#' with ls_paciente-nome_paciente into main_part.  "substitui valor
      endif.
      if sy-tabix < lines( it_paciente ).                                        "se nao for a ultima linha da tabela
        concatenate main_part row_part into main_part.                           "insere mais uma linha no html
      else.                                                                      "se chegar na ultima linha
        concatenate main_part end_part into main_part.                           "insere a tag de encerramento
      endif.
    endloop.

    lv_body_html = main_part.

  "--------------------------------------------------------------------------------------------

  data(lv_body_html_soli) = cl_bcs_convert=>string_to_soli( lv_body_html ).
  data(lo_multipart_ref) = new cl_gbt_multirelated_service( ).

  lo_multipart_ref->set_main_html(
      exporting
        content     = lv_body_html_soli
        description = 'Dados dos Pacientes' ).

  "substitui o html inicial pelo novo html tratado
  data(lo_doc_bcs_ref) = cl_document_bcs=>create_from_multirelated(
          exporting
            i_subject          = conv so_obj_des( lv_subject )
            i_importance       = lc_priority
            i_multirel_service = lo_multipart_ref ).

  lo_send_request_ref->set_document( lo_doc_bcs_ref  ).

  "insere o nome do remetente
  data(lo_sender_ref) = cl_sapuser_bcs=>create( sy-uname ).
  if lo_sender_ref is bound.
    lo_send_request_ref->set_sender( i_sender = lo_sender_ref ).
  endif.

  data(lo_recipient_ref) = cl_cam_address_bcs=>create_internet_address( p_email ).

  if lo_recipient_ref is bound.

    "adiciona o conteúdo à requisição
    lo_send_request_ref->add_recipient(
      exporting
        i_recipient = lo_recipient_ref
        i_express   = abap_true ).

  endif.

  "envia o email
  data(lv_sent_to_all) = lo_send_request_ref->send( ).

  "se o email for enviado, confirma a ação
  if lv_sent_to_all = abap_true.
    commit work and wait.
  endif.

endif.


"------------------------------------------------------------------------------------------------------------------------------------------------------
"RFC ZSEPARATE_HTML

FUNCTION ZSEPARATE_HTML.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(HTML_TEXT) TYPE  STRING
*"  EXPORTING
*"     REFERENCE(MAIN) TYPE  STRING
*"     REFERENCE(ROW)  TYPE  STRING
*"     REFERENCE(END)  TYPE  STRING
*"----------------------------------------------------------------------

  split html_text at '#endheader' into main row. "separa a linha da tabela do html principal
  split row at '#endrow' into row end.           "separa a linha do resto do html
  concatenate main row into main.                "insere a coluna novamente no html

ENDFUNCTION.
