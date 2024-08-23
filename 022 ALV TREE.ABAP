*&---------------------------------------------------------------------*
*& Report ZRLA_ALV_TREE_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zrla_alv_tree_02.

"estrutura principal
types: begin of wa_struct,
         nome_equipa type znn_equipa-nome_equipa,
         nome        type znn_candidatos-nome,
       end of wa_struct.

"estrutura do alv tree
types: begin of wa_struct2,
         nome type znn_candidatos-nome,
       end of wa_struct2.

"tabelas internas
data: it_data      type table of wa_struct2,
      it_data_tree type table of wa_struct.

"objetos de referencia
data: obj_tree     type ref to cl_salv_tree,
      obj_nodes    type ref to cl_salv_nodes,
      obj_node     type ref to cl_salv_node,
      obj_settings type ref to cl_salv_tree_settings.

"variavel para concatenar nomes às colunas do alv tree
data: lv_text type lvc_value.

start-of-selection.

  "implementa a tabela de referencia na arvore.
  call method cl_salv_tree=>factory
    importing
      r_salv_tree = obj_tree
    changing
      t_table     = it_data.

  "opcoes do cabecalho.
  obj_settings = obj_tree->get_tree_settings( ).
  obj_settings->set_header( 'Equipas Colaboradores' ).
  obj_settings->set_hierarchy_header( 'Equipas' ).
  obj_settings->set_hierarchy_tooltip( 'Test' ).
  obj_settings->set_hierarchy_size( 20 ).

  "leitura dos nodes
  obj_nodes = obj_tree->get_nodes( ).

  "consulta que devolve o nome da equipe e o nome do candidato
  select a~nome_equipa
         b~nome
    from znn_equipa_colab as c
    inner join znn_equipa as a
      on c~id_equipa = a~id_equipa
    inner join znn_candidatos as b
      on c~id_employee eq b~id_employee
    into corresponding fields of table it_data_tree
    .

  "criacao da tree e seus respectivos nodes
  loop at it_data_tree into data(wa_data).

    concatenate 'Equipa' wa_data-nome_equipa into lv_text separated by space.

    "instrucao para nao repetir os nomes das equipas presentes na tabela
    on change of wa_data-nome_equipa.

      "coluna principal com o nome das equipas
      obj_node = obj_nodes->add_node( related_node = ' '
                                      text = lv_text
                                      relationship = ' ' ).

      "criacao do node
      data(lv_order_key) = obj_node->get_key( ).
    endon.

    concatenate 'Módulo' wa_data-nome_equipa into lv_text separated by space.

    "nome dos colaboradores relacionados as equipas
    obj_node = obj_nodes->add_node( related_node = lv_order_key
                                    text = lv_text
                                    data_row = wa_data-nome
                                    relationship = cl_gui_column_tree=>relat_first_child ).

  endloop.

  "exibe o alv tree.
  obj_tree->display( ).
