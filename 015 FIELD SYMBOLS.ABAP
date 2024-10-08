"---------------------------------------------------------------------------------
[PERCORRENDO UMA TABELA INTERNA]

"structure
types: begin of ty_data,
  name type string,
  age type i,
end of ty_data.

"tabletype
types: ty_it_itab type standard table of ty_data with default key.

"internal table
data: it_itab type ty_it_itab.

"filling the table
it_itab = value ty_it_itab(

  ( name = 'tom' age = '20' )
  ( name = 'mike' age = '25' )
  ( name = 'joshua' age = '30' )
  ( name = 'maria' age = '35' )
  ( name = 'claire' age = '40' )

).

"declare field symbols
field-symbols: <fs_line> type ty_data.

"example 1
loop at it_itab assigning <fs_line>.
  write: / <fs_line>-name, <fs_line>-age.
endloop.
"---------------------------------------------------------------------------------
[PERCORRENDO UMA TABELA ESTRUTURA]

SELECT SINGLE FROM ekko
  FIELDS * INTO @DATA(wa_ekko).

DO.

  ASSIGN COMPONENT sy-index OF STRUCTURE wa_ekko TO FIELD-SYMBOL(<fs_value>).
  IF sy-subrc = 0.
    WRITE: / <fs_value>.
  ELSE.
    EXIT.
  ENDIF.

ENDDO.

"---------------------------------------------------------------------------------
[ESCREVENDO CAMPOS E VALORES DE UMA TABELA]

" Este relatório lê os campos da tabela EKKO a partir da tabela DD03L e exibe os nomes dos campos 
" e seus valores para cada registro na tabela EKKO.

" Seleciona todos os campos da tabela DD03L onde o nome da tabela é 'EKKO' e armazena os resultados em it_ekko_str
SELECT FROM dd03l
  FIELDS *
  WHERE tabname = 'EKKO'
    INTO TABLE @DATA(it_ekko_str).

" Seleciona todos os registros da tabela EKKO e armazena os resultados em it_ekko_data
SELECT FROM ekko
  FIELDS * INTO TABLE @DATA(it_ekko_data).

" Loop para percorrer todos os registros da tabela it_ekko_data
LOOP AT it_ekko_data INTO DATA(wa_data).
  " Loop para percorrer todos os campos da tabela it_ekko_str
  LOOP AT it_ekko_str ASSIGNING FIELD-SYMBOL(<fs_str>).
    " Atribui dinamicamente o valor do componente atual (campo) de wa_data a um field-symbol <fs_value>
    ASSIGN COMPONENT <fs_str>-fieldname OF STRUCTURE wa_data TO FIELD-SYMBOL(<fs_value>).
    
    " Verifica se a atribuição foi bem-sucedida
    IF sy-subrc EQ 0.
      " Escreve o nome do campo e o valor do campo na saída do relatório
      WRITE: / <fs_str>-fieldname, '=>', <fs_value>.
    ENDIF.
  ENDLOOP.
ENDLOOP.

"---------------------------------------------------------------------------------
[ALTERANDO TABELAS]

REPORT ZTEMP003_RAFAEL_FSYMBOLSSS.

"Field Symbols.
Field-Symbols: <lv_generic>  type any,       "qualquer variável
               <lit_generic> type any table, "qualquer tabela
               <lwa_t001>    type t001.      "estrutura da tabela t001.

data: it_t001 type table of t001. "tabela do tipo da tabelaz.

"select da tabela preenchendo a àrea de memoria a qual o apontador esta ligado,
"ou seja, preenche a trabela it_t001
select * from t001 into table <lit_generic>.

"loop a tabela interna, atribuindo a area de memoria de cada linha ao field sumbol.
loop at it_t001 assigning <lwa_t001>.
  <lwa_t001>-butxt = 'Nome sobregravado'. "Modifica a área de memoria que aponta para a alinha atual da tabela
endloop.

"declaracao inline + assign component
loop at it_t001 assigning field-symbol(<wa_inline>).
  assign component 'BUTXT' of structure <wa_inline> to field-symbol(<lv_inline>).
  <lv_inline> = 'Nome sobregravcado'.
endloop.
