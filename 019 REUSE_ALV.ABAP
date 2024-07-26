*&---------------------------------------------------------------------*
*& Report ZALV_SBX001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zalv_sbx001.

"tabela interna
DATA: it_t001 TYPE TABLE OF t001.

"catalogo de campo
DATA: it_fcat TYPE slis_t_fieldcat_alv.

START-OF-SELECTION.

  SELECT * FROM t001 INTO TABLE it_t001.

    "funcao para preencher o catalogo de campo
    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_program_name   = sy-repid
        i_structure_name = 'T001'
      CHANGING
        ct_fieldcat      = it_fcat.

    "remove o campo mandante do catalogo de campo
    LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<wa_fcat>).
      IF <wa_fcat>-fieldname EQ 'MANDT'.
        <wa_fcat>-no_out = abap_true.
      ENDIF.
    ENDLOOP.

*   "funcao para imprimir o alv a partir do catalogo
*   CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
*     EXPORTING
*       it_fieldcat = it_fcat
*     TABLES
*       t_outtab    = it_t001.

    "funcao para imprimir o alv a partir do catalogo em formato similar a excel
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        it_fieldcat = it_fcat
      TABLES
        t_outtab    = it_t001.
