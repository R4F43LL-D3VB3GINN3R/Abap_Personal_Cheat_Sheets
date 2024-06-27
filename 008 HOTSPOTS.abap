*&---------------------------------------------------------------------*
*& Report ZTEMP001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztemp001.

TYPES: BEGIN OF wa_vbak,
         vbeln TYPE vbak-vbeln,
       END OF wa_vbak.

DATA: t_output TYPE TABLE OF wa_vbak,
      ls_output TYPE wa_vbak.

SELECT vbeln FROM vbak INTO TABLE @t_output.

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
