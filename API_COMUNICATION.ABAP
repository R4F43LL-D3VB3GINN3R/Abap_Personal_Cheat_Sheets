REPORT zteste3.  
  
"---------------------------------------------------------------------------  
"---------------------------------------------------------------------------  
"---------------------------------------------------------------------------  
  
"url with parameter  
DATA(lv_url) = |https://airport-info.p.rapidapi.com/airport?iata=LAX|.  
  
"object client  
DATA: o_client TYPE REF TO if_http_client.  
  
"---------------------------------------------------------------------------  
"---------------------------------------------------------------------------  
"---------------------------------------------------------------------------  
  
START-OF-SELECTION.  
  
  PERFORM create_http_object.  
  PERFORM set_http.  
  PERFORM set_header_fields.  
  PERFORM set_timeout.  
  PERFORM read_response.  
  
END-OF-SELECTION.  
  
  "---------------------------------------------------------------------------  
  "---------------------------------------------------------------------------  
  "---------------------------------------------------------------------------  
  
FORM create_http_object.  
  
  cl_http_client=>create_by_url(  
  EXPORTING  
    url                    = lv_url " URL  
  IMPORTING  
    client                 = o_client " HTTP Client Abstraction  
  EXCEPTIONS  
    argument_not_found     = 1 " Communication Parameters (Host or Service) Not Available  
    plugin_not_active      = 2 " HTTP/HTTPS communication not available  
    internal_error         = 3 " Internal Error (e.g. name too long)  
    OTHERS                 = 4  
).  
  
  IF sy-subrc <> 0.  
    MESSAGE | Não foi possível estabelecer comunicação https | TYPE 'S' DISPLAY LIKE 'E'.  
    o_client->close( ).  
    RETURN.  
  ENDIF.  
  
ENDFORM.  
  
FORM set_http.  
  
  "send get request  
  IF o_client IS BOUND.  
    o_client->request->set_method( if_http_request=>co_request_method_get ).  
  ENDIF.  
  
ENDFORM.  
  
FORM set_header_fields.  
  
  o_client->request->set_header_field( name  = 'x-rapidapi-host'  
                                       value = 'airport-info.p.rapidapi.com' ).  
  
  o_client->request->set_header_field( name  = 'x-rapidapi-key'  
                                       value = 'fcbece6f52msh9348ce95ac4902ap18ec56jsna61eb515904a' ).  
  
ENDFORM.  
  
FORM set_timeout.  
  
  o_client->send( timeout = if_http_client=>co_timeout_default ).  
  
ENDFORM.  
  
FORM read_response.  
  
  DATA: lv_http_status TYPE i,  
        lv_status_text TYPE string.  
  
  o_client->receive( ).  
  o_client->response->get_status(  
   IMPORTING  
      code   = lv_http_status " HTTP Status Code  
      reason = lv_status_text " HTTP status description  
  ).  
  
  WRITE: / 'HTTP_STATUS_CODE:', lv_http_status.  
  WRITE: / 'STATUS_TEXT:', lv_status_text.  
  
  IF lv_http_status = 200.  
    DATA(lv_result) = o_client->response->get_cdata( ).  
    WRITE: / 'Response:'.  
    WRITE: / lv_result.  
    o_client->close( ).  
  ENDIF.  
  
ENDFORM.
