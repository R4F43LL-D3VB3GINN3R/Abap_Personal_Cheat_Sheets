"---------------------------------------------------------------
REPORT ZTEMP003_RAFAEL_FSYMBOLSS.
*"---------------------------------------------------------------
*"MAIN
types: begin of ty_t001,
  bukrs type t001-bukrs,
  butxt type t001-butxt,
end of ty_t001.
*
data: it_table type standard table of ty_t001 with header line.
data: lv_tablename type string.

lv_tablename = 'T001'.

data: lv_path type string,
      lv_filename type string,
      lv_extension type string.

"---------------------------------------------------------------
          "estrutura
"Import - "nome da tabela
          "tabela interna vazia
          "string com extensão do arquivo.

"IMPLEMENTACAO DA CLASSE

data: it_fieldnames type standard table of string.

data: lo_structdescr type ref to cl_abap_structdescr,
      lt_components  type cl_abap_structdescr=>component_table,
      ls_component   type line of cl_abap_structdescr=>component_table.

lo_structdescr ?= cl_abap_structdescr=>describe_by_data( it_table ).
lt_components = lo_structdescr->get_components( ).

LOOP AT lt_components INTO ls_component.
  append ls_component-name to it_fieldnames.
ENDLOOP.

DATA:  prog TYPE string,
       tab  TYPE STANDARD TABLE OF string,
       mess TYPE string,
       sid  TYPE string.

" Definição do subpool
APPEND 'PROGRAM subpool.'                      TO tab.
APPEND 'DATA gt_makt TYPE TABLE OF makt.'      TO tab.
APPEND 'LOAD-OF-PROGRAM.'                      TO tab.
APPEND '  SELECT matnr spras maktx'            TO tab.
APPEND '         FROM makt'                    TO tab.
APPEND '         INTO TABLE gt_makt.'          TO tab.
APPEND 'FORM loop_at_tab.'                     TO tab.
APPEND '  DATA gs_makt TYPE makt.'             TO tab.
APPEND '  LOOP AT gt_makt INTO gs_makt.'       TO tab.
APPEND '    WRITE: / gs_makt-matnr,'           TO tab.
APPEND '             gs_makt-spras,'           TO tab.
APPEND '             gs_makt-maktx.'           TO tab.
APPEND '  ENDLOOP.'                            TO tab.
APPEND 'ENDFORM.'                              TO tab.

" Geração do subpool
GENERATE SUBROUTINE POOL tab NAME prog
          MESSAGE mess
          SHORTDUMP-ID sid.

" Execução da sub-rotina gerada dinamicamente
IF sy-subrc = 0.
   PERFORM ('LOOP_AT_TAB') IN PROGRAM (prog) IF FOUND.
ELSEIF sy-subrc = 4.
   MESSAGE mess TYPE 'I'.
ELSEIF sy-subrc = 8.
   MESSAGE sid TYPE 'I'.
ENDIF.
