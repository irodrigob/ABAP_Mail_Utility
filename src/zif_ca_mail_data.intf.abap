INTERFACE zif_ca_mail_data
  PUBLIC .

  CONSTANTS: BEGIN OF cs_message,
               id      TYPE sy-msgid VALUE 'ZCA_MAIL',
               error   TYPE sy-msgty VALUE 'E',
               success TYPE sy-msgty VALUE 'S',
             END OF cs_message.

  CONSTANTS: BEGIN OF cs_mail,
               BEGIN OF section,
                 body    TYPE zca_e_mail_section VALUE 'B',
                 subject TYPE zca_e_mail_section VALUE 'S',
               END OF section,
               BEGIN OF type,
                 html TYPE so_obj_tp VALUE 'HTM',
               END OF type,
               BEGIN OF symbols,
                 start_symbol TYPE string VALUE '-&',
                 end_symbol   TYPE string VALUE '&-',
                 search_data  TYPE string VALUE '-&.*&-',
                 search_end   TYPE string VALUE '&-',
                 init_table   TYPE string VALUE '-&INIT_TABLE&-',
                 end_table    TYPE string VALUE '-&END_TABLE&-',
               END OF symbols,
               BEGIN OF send_confirmation,
                 only_error TYPE bcs_stml VALUE 'E',
                 read       TYPE bcs_stml VALUE 'R',
                 always     TYPE bcs_stml VALUE 'A',
                 sent       TYPE bcs_stml VALUE 'D',
                 never      TYPE bcs_stml VALUE 'N',
               END OF send_confirmation,
             END OF cs_mail.

ENDINTERFACE.
