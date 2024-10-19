report zteste1.

data: it_sflight type table of sflight.
data: it_string type string.

start-of-selection.

select * from sflight into table it_sflight.

it_string = /ui2/cl_json=>serialize( it_sflight ).

cl_demo_output=>display_data( it_string ).
