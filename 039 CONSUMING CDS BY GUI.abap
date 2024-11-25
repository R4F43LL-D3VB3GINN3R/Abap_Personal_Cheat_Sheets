*&---------------------------------------------------------------------*
*& Report ZCDS_VIEW
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zcds_view.

"objetos de trabalho
data: ol_alv   type ref to cl_salv_table,
      it_table type table of sflight.

"objetos de classe de leitura de cds
data: go_alv_ida    type ref to if_salv_gui_table_ida,
      go_fullscreen type ref to if_salv_gui_fullscreen_ida.

selection-screen: begin of block a1 with frame.
parameters: rb1 radiobutton group grp1 default 'X', "alv simples da cds a partir de consulta direta na view
            rb2 radiobutton group grp1,             "alv a partir de metodo da classe if_salv_gui_table_ida
            rb3 radiobutton group grp1.             "alv a partir do método da classe if_salv_gui_table executado diretamente com uma linha
selection-screen: end of block a1.

start-of-selection.

  if rb1 eq 'X'.
    perform display_data.
  elseif rb2 eq 'X'.
    perform display_data2.
  elseif rb3 eq 'X'.
    perform display_data3.
  endif.

form display_data.

  select *
    from zv_voo
    into table it_table.

  cl_salv_table=>factory(
    importing
      r_salv_table   = ol_alv                           " Basis Class Simple ALV Tables
    changing
      t_table        = it_table
  ).

  ol_alv->display( ).

endform.

form display_data2.

  "metodo recebe o nome da define view e nao o sqlviewname
  go_alv_ida =  cl_salv_gui_table_ida=>create_for_cds_view(
                  iv_cds_view_name      = 'ZDD_VOO' ).

  go_fullscreen = go_alv_ida->fullscreen( ).
  go_fullscreen->display( ).

endform.

form display_data3.

  "metodo faz a mesma coisa do display data2, mas usando de apenas uma linha.
  cl_salv_gui_table_ida=>create_for_cds_view( 'ZDD_VOO')->fullscreen( )->display( ).

endform.
