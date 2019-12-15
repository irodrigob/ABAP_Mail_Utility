*&---------------------------------------------------------------------*
*& Report zca_mail_r_simple_template
*&---------------------------------------------------------------------*
*& Example of using simple template in mail using the class ZCL_CA_SEND_MAIL
*& Value of subject field: User -&USER&- this is a example of simple template.
*& Value of body field:
*& User -&USER&- this is a example of simple template.
*&---------------------------------------------------------------------*
REPORT zca_mail_r_simple_template.



START-OF-SELECTION.


  DATA(lo_mail) = NEW zcl_ca_send_mail(  ).


  " The symbol must start with -& and end with &-
  DATA(lt_symbols) = VALUE zca_i_mail_template_symbols( ( symbol = '-&USER&-' value = |{ sy-uname }| ) ).

  " You can also use the following method to add the symbols
*  zcl_ca_send_mail=>set_symbols_mail(
*    EXPORTING
*      iv_name         = |USER|
*      iv_value        = |{ sy-uname }|
*      iv_table        = abap_false
*    CHANGING
*      ct_symbols_mail = lt_symbols ).


  lo_mail->send_with_template( EXPORTING iv_template         = CONV zca_e_ttempl_name( |SIMPLE| )
                                         it_recipients       = VALUE #( ( |nobody@testing_mail_invent.com| ) )
                                         iv_sender           = |noreplynobody@testing_mail_invent.com|
                                         iv_appl             = CONV zca_e_ttempl_appl( |EXAMPLE| )
                                         it_symbols          = lt_symbols
                               IMPORTING es_return           = DATA(ls_return) ).

  IF ls_return-type NE zif_ca_mail_data=>cs_message-error.
    WRITE:/ 'Mail sended, go to the SOST transaction to see it'.
  ELSE.
    WRITE:/ ls_return-message.
  ENDIF.
