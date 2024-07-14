"------------------------------------------------------------------------------------

"estructure with t001
TYPES: BEGIN OF ty_t001,
  bukrs TYPE t001-bukrs, "Company Code
  butxt TYPE t001-butxt, "Name of Company Code or Company
END OF ty_t001.

"------------------------------------------------------------------------------------

"excel columns
DATA: it_fieldnames TYPE TABLE OF string. "internal table - column names

APPEND 'BUKRS' TO it_fieldnames.
APPEND 'BUTXT' TO it_fieldnames.

"------------------------------------------------------------------------------------

"fill table with t001 data
DATA: it_t001 TYPE TABLE OF ty_t001, "internal table
      ls_t001 TYPE ty_t001.          "local structure

"searching for brazilian companies
SELECT bukrs,
       butxt
  FROM t001
  INTO CORRESPONDING FIELDS OF TABLE @it_t001
  WHERE land1 = 'BR'.

"------------------------------------------------------------------------------------

"output table >> processing data
DATA: it_output TYPE TABLE OF ty_t001 WITH HEADER LINE. "export table to receive xls

LOOP AT it_fieldnames INTO DATA(lv_fieldname).
  ASSIGN COMPONENT lv_fieldname OF STRUCTURE ls_t001 TO FIELD-SYMBOL(<fs_value>).
  <fs_value> = lv_fieldname.
ENDLOOP.

"inserts the header on it_output
APPEND ls_t001 TO it_output.
"inserts the lines below the header
APPEND LINES OF it_t001 TO it_output.

"------------------------------------------------------------------------------------

"it_output >> export xls.
CALL FUNCTION 'GUI_DOWNLOAD'
  EXPORTING
    filename                        = 'C:\Users\rafal\Desktop\file.xls'
    filetype                        = 'ASC'
    write_field_separator           = 'X'
  TABLES
    data_tab                        = it_output.

"------------------------------------------------------------------------------------
