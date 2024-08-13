types: begin of stu_porders,
         aufnr type aufnr,
         aufpl type co_aufpl,
         vornr type vornr,
       end of stu_porders.

data: it_data type table of stu_porders.

data: obj_tree     type ref to cl_salv_tree,
      obj_nodes    type ref to cl_salv_nodes,
      obj_node     type ref to cl_salv_node,
      obj_settings type ref to cl_salv_tree_settings.

" step 1 - get ref obj tree.
call method cl_salv_tree=>factory
  importing
    r_salv_tree = obj_tree
  changing
    t_table     = it_data.

obj_settings = obj_tree->get_tree_settings( ).

obj_settings->set_header( 'ALV Tree Samples' ).
obj_settings->set_hierarchy_header( 'Order Hir' ).
obj_settings->set_hierarchy_tooltip( 'Test' ).
obj_settings->set_hierarchy_size( 20 ).

obj_nodes = obj_tree->get_nodes( ).

do 5 times.

  obj_node = obj_nodes->add_node( related_node = ' '
                                  text = 'Parent 1'
                                  relationship = ' ' ).

  data(lv_parent_key) = obj_node->get_key( ).

  obj_node = obj_nodes->add_node( related_node = lv_parent_key
                                  text = 'Child 1'
                                  relationship = cl_gui_column_tree=>relat_first_child ).

enddo.

obj_tree->display( ).
