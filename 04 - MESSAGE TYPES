01 - BUILDING A MESSAGE
.....................................................................................................................................................

DATA: LV_MESSAGE_POPUP TYPE SY-MSGV1. "The variable is declared to be of type system-message"

LV_MESSAGE_POPUP = 'LOOK MY POPUP'. "The variable is given a value"

MESSAGE i001(00) WITH LV_MESSAGE_POPUP DISPLAY LIKE 'I'. "The statement MESSAGE is used to display a message...
                                                         "i001 is the message's ID of pattern information of type 'I'...
                                                         "(00) indicate that it is useds like a default text to this message...
                                                         "WITH LV_MESSAGE_POPUP my variable is being used like a normal text to this message...
                                                         "DISPLAY LIKE 'I' statement to display the message like a popup.
WRITE: 'END'.
......................................................................................................................................................
......................................................................................................................................................

02 - DIFFERENT TYPES OS MESSAGE
......................................................................................................................................................

DATA LV_MESSAGE_1 TYPE SY-MSGV1.

LV_MESSAGE_1 = 'ERROR'.

MESSAGE E001(00) WITH LV_MESSAGE_1 DISPLAY LIKE 'I' "INFORMATION POPUP"

WRITE 'END'.

--

DATA LV_MESSAGE_1 TYPE SY-MSGV1.

LV_MESSAGE_1 = 'ERROR'.

MESSAGE E001(00) WITH LV_MESSAGE_1 DISPLAY LIKE 'E'. "ERROR SCREEN BELOW"

WRITE 'END'.

--

DATA: LV_MESSAGE_1 TYPE SY-MSGV1.

LV_MESSAGE_1 = 'CAUTION'.

MESSAGE I001(00) WITH LV_MESSAGE_1 DISPLAY LIKE 'W'. "CAUTION POPUP"

WRITE 'END'.

--

DATA LV_MESSAGE_1 TYPE SY-MSGV1.

LV_MESSAGE_1 = 'S'.

MESSAGE I001(00) WITH LV_MESSAGE_1 DISPLAY LIKE 'S'. "SUCESS POPUP"

WRITE 'END'.
......................................................................................................................................................
......................................................................................................................................................
