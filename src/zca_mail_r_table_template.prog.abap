*&---------------------------------------------------------------------*
*& Report zca_mail_r_table_template
*&---------------------------------------------------------------------*
*& Example of using simple template in mail using the class ZCL_CA_SEND_MAIL
*& Value of subject field: User -&USER&- example of table in template
*& Value of body field:
*& User -&USER&- example of table in template
*&
*&---------------------------------------------------------------------*
REPORT zca_mail_r_table_template.
DATA lt_symbols TYPE zca_i_mail_template_symbols.


START-OF-SELECTION.


  DATA(lo_mail) = NEW zcl_ca_send_mail(  ).

  " Normal symbol
  zcl_ca_send_mail=>set_symbols_mail(
    EXPORTING
      iv_name         = |USER|
      iv_value        = |{ sy-uname }|
      iv_table        = abap_false
    CHANGING
      ct_symbols_mail = lt_symbols ).

  " Symbols table
  " The symbol must start with -& and end with &-

  "Se añade la claves que se usarán para mapear los valores insertados
  DATA(lt_table_symbols) = VALUE zca_i_mail_table_symbols_value( ( key = |ROW_1| ) ).

  " Se añade los valores al simbolo y clave
  zcl_ca_send_mail=>set_symbols_mail(
  EXPORTING
    iv_name         = |T_VALUE1|
    iv_value        = |{ sy-uzeit TIME = ENVIRONMENT }|
    iv_table        = abap_true
    iv_table_key = |ROW_1|
  CHANGING
    ct_symbols_mail = lt_symbols ).

  zcl_ca_send_mail=>set_symbols_mail(
  EXPORTING
    iv_name         = |T_VALUE2|
    iv_value        = |{ sy-datum DATE = ENVIRONMENT }|
    iv_table        = abap_true
    iv_table_key = |ROW_1|
  CHANGING
    ct_symbols_mail = lt_symbols ).


  lo_mail->send_with_template( EXPORTING iv_template         = CONV zca_e_ttempl_name( |TABLE| )
                                         it_recipients       = VALUE #( ( |nobody@testing_mail_invent.com| ) )
                                         iv_sender           = |noreplynobody@testing_mail_invent.com|
                                         iv_appl             = CONV zca_e_ttempl_appl( |EXAMPLE| )
                                         it_symbols          = lt_symbols
                                         it_symbols_in_table = lt_table_symbols
                               IMPORTING es_return           = DATA(ls_return) ).

  IF ls_return-type NE zif_ca_mail_data=>cs_message-error.
    WRITE:/ 'Mail sended, go to the SOST transaction to see it'.
  ELSE.
    WRITE:/ ls_return-message.
  ENDIF.
