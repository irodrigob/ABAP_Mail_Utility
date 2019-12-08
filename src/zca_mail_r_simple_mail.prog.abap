*&---------------------------------------------------------------------*
*& Report zca_mail_r_simple_mail
*&---------------------------------------------------------------------*
*& Example of simple mail using the class ZCL_CA_SEND_MAIL
*&---------------------------------------------------------------------*
REPORT zca_mail_r_simple_mail.



START-OF-SELECTION.


  DATA(lo_mail) = NEW zcl_ca_send_mail(  ).

  lo_mail->send(
    EXPORTING
      it_recipients       = VALUE #( ( |nobody@testing_mail_invent.com| ) )
      iv_sender           = |noreplynobody@testing_mail_invent.com|
      iv_body             = |body mail|
      iv_subject          = |subject|
    IMPORTING
      es_return           = DATA(ls_return) ).

  IF ls_return-type NE zif_ca_mail_data=>cs_message-error.
    WRITE:/ 'Mail sended, go to the SOST transaction to see it'.
  ELSE.
    WRITE:/ ls_return-message.
  ENDIF.
