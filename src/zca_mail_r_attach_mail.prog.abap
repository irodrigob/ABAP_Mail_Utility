*&---------------------------------------------------------------------*
*& Report zca_mail_r_attach_mail
*&---------------------------------------------------------------------*
*& Example of mail with attachment using the class ZCL_CA_SEND_MAIL
*&---------------------------------------------------------------------*
REPORT zca_mail_r_attach_mail.

DATA mt_filetable TYPE filetable.
DATA mt_file_bin TYPE solix_tab.
DATA mv_file_content TYPE xstring.
DATA mv_file_name TYPE string.

PARAMETERS p_file TYPE rlgrap-filename.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA lv_rc TYPE i.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      multiselection = abap_false
      file_filter    = '*.*'
    CHANGING
      file_table     = mt_filetable
      rc             = lv_rc.

  READ TABLE mt_filetable INTO p_file INDEX 1.

START-OF-SELECTION.

* " Upload file
  cl_gui_frontend_services=>gui_upload(
     EXPORTING
       filename                = CONV string( p_file )
       filetype                = 'BIN'
  IMPORTING
       filelength              = DATA(mv_len)
     CHANGING
       data_tab                = mt_file_bin
     EXCEPTIONS
       file_open_error         = 1
       file_read_error         = 2
       no_batch                = 3
       gui_refuse_filetransfer = 4
       invalid_type            = 5
       no_authority            = 6
       unknown_error           = 7
       bad_data_format         = 8
       header_not_allowed      = 9
       separator_not_allowed   = 10
       header_too_long         = 11
       unknown_dp_error        = 12
       access_denied           = 13
       dp_out_of_memory        = 14
       disk_full               = 15
       dp_timeout              = 16
       not_supported_by_gui    = 17
       error_no_gui            = 18
       OTHERS                  = 19
   ).
  IF sy-subrc <> 0.
    WRITE:/ 'Error to read the file'.
  ELSE.


    " Convert binary file to xstring
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = mv_len
      IMPORTING
        buffer       = mv_file_content
      TABLES
        binary_tab   = mt_file_bin
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    " Get the name of file
    CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = p_file
      IMPORTING
        stripped_name = mv_file_name.

    " get the file extension
    SPLIT mv_file_name AT '.' INTO: DATA(lv_name) DATA(lv_extension).

    DATA(lo_mail) = NEW zcl_ca_send_mail(  ).

    lo_mail->send(
      EXPORTING
        it_recipients       = VALUE #( ( |nobody@testing_mail_invent.com| ) )
        iv_sender           = |noreplynobody@testing_mail_invent.com|
        iv_body             = |mail with attachments|
        iv_subject          = |subject with attach|
        it_attachs = VALUE #( ( name = mv_file_name type = |{ lv_extension CASE = UPPER }| content_bin = mv_file_content ) )
      IMPORTING
        es_return           = DATA(ls_return) ).

    IF ls_return-type NE zif_ca_mail_data=>cs_message-error.
      WRITE:/ 'Mail sended, go to the SOST transaction to see it'.
    ELSE.
      WRITE:/ ls_return-message.
    ENDIF.

  ENDIF.
