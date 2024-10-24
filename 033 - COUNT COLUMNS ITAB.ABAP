  data: itab type table of mara.

      data: lo_table_descr  type ref to cl_abap_tabledescr,
          lo_struct_descr type ref to cl_abap_structdescr.

*     Use RTTI services to describe table variable
    lo_table_descr ?= cl_abap_tabledescr=>describe_by_data( p_data = itab ).
*     Use RTTI services to describe table structure
    lo_struct_descr ?= lo_table_descr->get_table_line_type( ).

*     Count number of columns in structure
    data(lv_number_of_columns) = lines( lo_struct_descr->components ).
