cl_demo_output: Esta é uma classe de demonstração fornecida pelo sistema SAP para facilitar a exibição de dados, especialmente útil em programas de teste ou demonstrações.
=>display: Este é um método estático da classe cl_demo_output usado para exibir o conteúdo que é passado para ele.

*&---------------------------------------------------------------------*
*& Report ZTEMP001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztemp001.

data: lt_vbak type table of vbak.

SELECT * FROM vbak INTO TABLE lt_vbak up to 10 rows.

"------------------------------------------------------------------------

*cl_demo_output=>display( carriers ). "Most Simple and Common Usage

"------------------------------------------------------------------------

*
*CALL TRANSFORMATION id SOURCE vbak = lt_vbak "Transforms data table in xml
*                       RESULT XML DATA(xml).
*
*cl_demo_output=>new(
*  )->begin_section( `Some Text`
*  )->write_text( |blah blah blah \n| &&
*                 |blah blah blah|
*  )->next_section( `Some Data`
*  )->begin_section( `Elementary Object`
*  )->write_data( lt_vbak[ 1 ]-vbeln
*  )->next_section( `Internal Table`
*  )->write_data( lt_vbak
*  )->end_section(
*  )->next_section( `XML`
*  )->write_xml( xml
*  )->display( ).

"------------------------------------------------------------------------

*cl_demo_output=>new( 'TEXT' "Text Mode and more Simplist.
*  )->display( lt_vbak ).

"------------------------------------------------------------------------


