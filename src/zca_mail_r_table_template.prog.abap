*&---------------------------------------------------------------------*
*& Report zca_mail_r_table_template
*&---------------------------------------------------------------------*
*& Example of using simple template in mail using the class ZCL_CA_SEND_MAIL
*& Value of subject field: User -&USER&- example of table in template
*& Value of body field(HTML format:
*<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
*<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" ""><HTML><HEAD>
*
*<META http-equiv="content-type" content="text/html; charset=utf-8">
*<META name="GENERATOR" content="MSHTML 11.00.10570.1001">
*<STYLE> table, th, td { border: 1px solid black; border-collapse: collapse; padding: 0px; }
*.cl_parag1 { font-family: Calibri, Helvetica; font-size: 14px; }  </STYLE>
*      </HEAD>
*<BODY>
*<P>User -&amp;USER&amp;- example of table in template</P>
*<P><BR></P>
*<TABLE width="80%">
*  <TBODY>
*  <TR>
*    <TH class="cl_parag1">Value 1</TH>
*    <TH class="cl_parag1">Value 2</TH>
*    <TH class="cl_parag1">Value 3</TH><!---&INIT_TABLE&--->
*
*  <TR>
*    <TD class="cl_parag1">-&amp;T_VALUE1&amp;-</TD>
*    <TD class="cl_parag1">-&amp;T_VALUE2&amp;-</TD>
*    <TD class="cl_parag1">-&amp;USER&amp;-</TD></TR><!---&END_TABLE&--->
*       </TBODY></TABLE></BODY></HTML>
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

  " Se añade los valores al simbolo y clave
  zcl_ca_send_mail=>set_symbols_mail(
  EXPORTING
    iv_name         = |T_VALUE1|
    iv_value        = |{ sy-uzeit TIME = ENVIRONMENT }|
    iv_row_table = 1
  CHANGING
    ct_symbols_mail = lt_symbols ).

  zcl_ca_send_mail=>set_symbols_mail(
  EXPORTING
    iv_name         = |T_VALUE2|
    iv_value        = |{ sy-datum DATE = ENVIRONMENT }|
    iv_row_table = 1
  CHANGING
    ct_symbols_mail = lt_symbols ).


  lo_mail->send_with_template( EXPORTING iv_template         = CONV zca_e_ttempl_name( |TABLE| )
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
