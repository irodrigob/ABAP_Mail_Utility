CLASS zcl_ca_send_mail DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_langu TYPE sylangu DEFAULT sy-langu .
    METHODS send_with_template
      IMPORTING
        !iv_langu            TYPE sy-langu DEFAULT sy-langu
        !it_attachs          TYPE zca_i_mail_attach OPTIONAL
        !iv_template         TYPE zca_e_ttempl_name
        !it_recipients       TYPE bcsy_smtpa
        !it_recipients_cc    TYPE bcsy_smtpa OPTIONAL
        !it_recipients_bcc   TYPE bcsy_smtpa OPTIONAL
        !iv_sender           TYPE ad_smtpadr OPTIONAL
        !it_symbols          TYPE zca_i_mail_template_symbols OPTIONAL
        !it_symbols_in_table TYPE zca_i_mail_table_symbols_value OPTIONAL
        !iv_request_lecture  TYPE sap_bool DEFAULT abap_false
        !iv_commit           TYPE sap_bool DEFAULT abap_true
        !iv_replyto          TYPE ad_smtpadr OPTIONAL
        !iv_appl             TYPE any
        !iv_set_long_subjet  TYPE sap_bool DEFAULT abap_false
        !it_images           TYPE zca_i_mail_images OPTIONAL
      EXPORTING
        !es_return           TYPE bapiret2
        !ev_internal_mail_id TYPE char100 .
    "! <p class="shorttext synchronized" lang="en">Send mail without template</p>
    "!
    "! @parameter it_images | <p class="shorttext synchronized" lang="en">Images that are embedded in the body</p>
    "! @parameter it_attachs | <p class="shorttext synchronized" lang="en">Attachs</p>
    "! @parameter it_recipients | <p class="shorttext synchronized" lang="en">Recipients</p>
    "! @parameter it_recipients_cc | <p class="shorttext synchronized" lang="en">Recipients in copy</p>
    "! @parameter it_recipients_bcc | <p class="shorttext synchronized" lang="en">Recipients in hidden copy</p>
    "! @parameter iv_sender | <p class="shorttext synchronized" lang="en">Sender</p>
    "! @parameter it_symbols | <p class="shorttext synchronized" lang="en">Normal symbols that are replaced in the subject and body</p>
    "! @parameter it_symbols_in_table | <p class="shorttext synchronized" lang="en">Symbols that are replaced in the body to build HTML tables</p>
    "! @parameter iv_request_lecture | <p class="shorttext synchronized" lang="en">Request lecture</p>
    "! @parameter iv_commit | <p class="shorttext synchronized" lang="en">Commit after send mail</p>
    "! @parameter iv_replyto | <p class="shorttext synchronized" lang="en">Reply-to</p>
    "! @parameter iv_appl | <p class="shorttext synchronized" lang="en">Appl¿?</p>
    "! @parameter iv_set_long_subjet | <p class="shorttext synchronized" lang="en">The subject exceeded 50 characters</p>
    "! @parameter iv_body | <p class="shorttext synchronized" lang="en">Body</p>
    "! @parameter iv_subject | <p class="shorttext synchronized" lang="en">Subject</p>
    "! @parameter iv_signature | <p class="shorttext synchronized" lang="en">Signature</p>
    "! @parameter iv_preview | <p class="shorttext synchronized" lang="en">Preview</p>
    "! @parameter es_return | <p class="shorttext synchronized" lang="en"Proces return</p>
    "! @parameter ev_internal_mail_id | <p class="shorttext synchronized" lang="en">Internal ID of mail</p>
    "! @parameter ev_body | <p class="shorttext synchronized" lang="en">Body with the symbols already replaced</p>
    "! @parameter ev_subject | <p class="shorttext synchronized" lang="en">Subject with the symbols already replaced</p>
    METHODS send
      IMPORTING
        !it_images           TYPE zca_i_mail_images OPTIONAL
        !it_attachs          TYPE zca_i_mail_attach OPTIONAL
        !it_recipients       TYPE bcsy_smtpa
        !it_recipients_cc    TYPE bcsy_smtpa OPTIONAL
        !it_recipients_bcc   TYPE bcsy_smtpa OPTIONAL
        !iv_sender           TYPE ad_smtpadr OPTIONAL
        !it_symbols          TYPE zca_i_mail_template_symbols OPTIONAL
        !it_symbols_in_table TYPE zca_i_mail_table_symbols_value OPTIONAL
        !iv_request_lecture  TYPE sap_bool DEFAULT abap_false
        !iv_commit           TYPE sap_bool DEFAULT abap_true
        !iv_replyto          TYPE ad_smtpadr OPTIONAL
        !iv_appl             TYPE any OPTIONAL
        !iv_set_long_subjet  TYPE sap_bool DEFAULT abap_false
        !iv_body             TYPE string
        !iv_subject          TYPE string
        !iv_signature        TYPE string OPTIONAL
        !iv_preview          TYPE sap_bool DEFAULT abap_false
      EXPORTING
        !es_return           TYPE bapiret2
        !ev_internal_mail_id TYPE char100
        !ev_body             TYPE string
        !ev_subject          TYPE string .
    CLASS-METHODS set_symbols_mail
      IMPORTING
        !iv_name         TYPE bsstring
        !iv_table_key    TYPE bsstring OPTIONAL
        !iv_value        TYPE bsstring
        !iv_table        TYPE sap_bool DEFAULT abap_false
      CHANGING
        !ct_symbols_mail TYPE zca_i_mail_template_symbols .
    CLASS-METHODS set_structure_symbols
      IMPORTING
        !is_structure    TYPE any
      CHANGING
        !ct_symbols_mail TYPE zca_i_mail_template_symbols .
  PROTECTED SECTION.

    TYPES: tt_body TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DATA mo_mail TYPE REF TO cl_bcs .
    DATA mo_doc_bcs TYPE REF TO cl_document_bcs .
    DATA mv_subject TYPE string .
    DATA mt_body TYPE tt_body .
    DATA mv_langu TYPE sylangu .
*  data mO_PLANTILLA type ref to ZCL_CA_PLANTILLA_MAIL .
    DATA mo_mime_helper TYPE REF TO cl_gbt_multirelated_service .

    "! <p class="shorttext synchronized" lang="en">Set the subject and body</p>
    "! Set the subject and body. Replacing the symbols that are passed by parameter
    "! @parameter it_symbols | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter it_symbols_in_table | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter es_return | <p class="shorttext synchronized" lang="en"></p>
    METHODS set_subject_body
      IMPORTING
        it_symbols          TYPE zca_i_mail_template_symbols OPTIONAL
        it_symbols_in_table TYPE zca_i_mail_table_symbols_value OPTIONAL
      EXPORTING
        es_return           TYPE bapiret2.

    "! <p class="shorttext synchronized" lang="en">Replace normal symbols and symbols in table</p>
    "! Replace normal symbols and symbols to build HTML tables
    "! @parameter it_symbols | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter it_symbols_in_table | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ct_body | <p class="shorttext synchronized" lang="en"></p>
    METHODS replace_symbols_in_table
      IMPORTING
        it_symbols          TYPE zca_i_mail_template_symbols
        it_symbols_in_table TYPE zca_i_mail_table_symbols_value
      CHANGING
        ct_body             TYPE zcl_ca_send_mail=>tt_body.
    METHODS generate_dynamic_symbol_table
      IMPORTING
        it_symbols          TYPE zca_i_mail_template_symbols
        it_symbols_in_table TYPE zca_i_mail_table_symbols_value
      EXPORTING
        eo_table_data       TYPE REF TO data
        eo_data_struct      TYPE REF TO data.
    METHODS remove_special_chars
      IMPORTING
        iv_symbol        TYPE string
      RETURNING
        VALUE(rv_symbol) TYPE string.
    METHODS apply_data_to_template
      IMPORTING
        iv_html_string        TYPE string
        it_data               TYPE INDEX TABLE OPTIONAL
        is_general_data       TYPE any
      RETURNING
        VALUE(rv_html_string) TYPE string.
    METHODS remove_html_comment
      IMPORTING
        iv_in_string         TYPE string
      RETURNING
        VALUE(rv_out_string) TYPE string.
    "! <p class="shorttext synchronized" lang="en">Set recipients to the mail</p>
    "!
    METHODS set_recipientes
      IMPORTING
        it_recipients     TYPE bcsy_smtpa
        it_recipients_cc  TYPE bcsy_smtpa
        it_recipients_bcc TYPE bcsy_smtpa
      EXPORTING
        es_return         TYPE bapiret2.
    "! <p class="shorttext synchronized" lang="en">Set sender to the mail</p>
    "!
    METHODS set_sender
      IMPORTING
        iv_sender TYPE ad_smtpadr.
    METHODS set_replyto
      IMPORTING
        iv_replyto TYPE ad_smtpadr.
    METHODS set_images
      IMPORTING
        it_images TYPE zca_i_mail_images.
    METHODS set_subject_body_with_mail
      IMPORTING
        it_images           TYPE zca_i_mail_images
        it_symbols          TYPE zca_i_mail_template_symbols
        it_symbols_in_table TYPE zca_i_mail_table_symbols_value
      EXPORTING
        es_return           TYPE bapiret2.
    METHODS replace_symbols_body_subject
      IMPORTING
        it_symbols          TYPE zca_i_mail_template_symbols
        it_symbols_in_table TYPE zca_i_mail_table_symbols_value
      EXPORTING
        es_return           TYPE bapiret2.
    METHODS convert_body_2_bcs
      RETURNING
        VALUE(rt_body) TYPE bcsy_text.
    METHODS convert_subject_2_bcs
      RETURNING
        VALUE(rv_subject) TYPE so_obj_des.
    METHODS set_attachs
      IMPORTING
        it_attachs TYPE zca_i_mail_attach
      EXPORTING
        es_return  TYPE bapiret2.
    METHODS set_attributes.
    METHODS get_internal_mail_id
      EXPORTING
        ev_internal_mail_id TYPE char100.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_ca_send_mail IMPLEMENTATION.


  METHOD apply_data_to_template.

    DATA: lv_html_string      TYPE string,
          lv_field_name       TYPE string,

          lv_start_off        TYPE i,
          lv_end_len          TYPE i,
          lv_pos_found        TYPE i,
          lv_last_pos         TYPE i,
          lv_substring        TYPE string,
          lv_substring_aux    TYPE string,
          lv_sub_len          TYPE i,
          lv_end_token        TYPE i,
          lv_token            TYPE string,
          lv_token_length     TYPE i,
          lv_field_string     TYPE string,
          lv_end_table        TYPE i,
          lv_table_string     TYPE string,
          lv_form_string      TYPE string,
          lv_end_token_lenght TYPE i.

    DATA: lv_anyadir TYPE flag.
    DATA: lv_es_plantilla TYPE flag.

    FIELD-SYMBOLS: <ls_data>       TYPE any,
                   <ls_table_line> TYPE any,
                   <comp>          TYPE any.

    CLEAR: rv_html_string.

* We read the first value just in case:
*    READ TABLE it_data ASSIGNING <ls_data> INDEX 1.
    ASSIGN is_general_data TO <ls_data>.

* we get the original code as a string
    lv_html_string = iv_html_string.


* we initialize the variables:
    lv_last_pos         = strlen( lv_html_string ).
    lv_end_token_lenght = strlen( zif_ca_mail_data=>cs_mail-symbols-search_end ) - 1.

    CLEAR: lv_start_off.
    lv_end_len = lv_last_pos.

    WHILE lv_start_off LT lv_last_pos.
*   Buscamos la siguiente cadena de comentario:
      lv_pos_found = find( val   = lv_html_string
                           regex = zif_ca_mail_data=>cs_mail-symbols-search_data
                           case  = abap_false
                           off   = lv_start_off
                           len   = lv_end_len ).

*   En caso que ya no haya mas nos quedaremos con el resto del string
      IF lv_pos_found LT 0.
        lv_sub_len = lv_last_pos - lv_start_off.
      ELSE.
        lv_sub_len = lv_pos_found - lv_start_off.
      ENDIF.

*   Nos quedamos con el substring a insertar:
      lv_substring = substring( val   = lv_html_string
                                off   = lv_start_off
                                len   = lv_sub_len ).

      IF lv_pos_found GE 0.
*     Y tambien nos quedamos con el token:
        lv_end_token = find( val  = lv_html_string
                             sub  = zif_ca_mail_data=>cs_mail-symbols-search_end
                             case = abap_false
                             off  = lv_pos_found ).

        lv_token     = substring( val   = lv_html_string
                                  off   = lv_pos_found
                                  len   = lv_end_token - lv_pos_found + 1 + lv_end_token_lenght ).


        lv_token_length = strlen( lv_token ).

*     lo tratamos:
        CASE lv_token.
          WHEN zif_ca_mail_data=>cs_mail-symbols-init_table.
*         Tenemos que buscar el token de fin de tabla:
            lv_end_table = find( val  = lv_html_string
                                 sub  = zif_ca_mail_data=>cs_mail-symbols-end_table
                                 case = abap_false
                                 off  = lv_pos_found ).

            lv_table_string = substring( val   = lv_html_string
                                         off   = lv_pos_found + lv_token_length
                                         len   = lv_end_table - lv_pos_found - lv_token_length ).

            LOOP AT it_data ASSIGNING <ls_table_line>.
              CLEAR: lv_anyadir. " INSR JZA Si es multitabla, eliminar los registros en blanco.
              apply_data_to_template( EXPORTING iv_html_string  = lv_table_string
                                                is_general_data = <ls_table_line>
                                      RECEIVING rv_html_string  = lv_form_string ).

              lv_substring = |{ lv_substring }{ lv_form_string }|.
              lv_anyadir = abap_true. " INSR JZA Si es multitabla, eliminar los registros en blanco.
            ENDLOOP.

            lv_token_length = lv_end_table - lv_pos_found.
          WHEN zif_ca_mail_data=>cs_mail-symbols-end_table.
*         do nothing
          WHEN OTHERS.
            lv_field_string = remove_html_comment( lv_token ).
            lv_field_name   = remove_special_chars( lv_field_string ).
            IF <ls_data> IS ASSIGNED.
              lv_es_plantilla = abap_true. "marcamos que tiene variables
              UNASSIGN <comp>.
              ASSIGN COMPONENT lv_field_name OF STRUCTURE <ls_data>
                TO <comp>.
              IF <comp> IS ASSIGNED.
                lv_substring = |{ lv_substring }{ <comp> }|.
* Si es multitabla, eliminar los registros en blanco.
                IF <comp> IS NOT INITIAL.
                  lv_anyadir = abap_true.
                ENDIF.

              ENDIF.
            ENDIF.
        ENDCASE.

        "En el caso de que el mail no tenga plantilla, antes del cambio
        "no se informaba el cuerpo del correo

* Cuando termina de buscar añade las lineas en blanco siempre
      ELSE.
        IF lv_es_plantilla = abap_false.
          lv_anyadir = abap_true.
        ENDIF.
      ENDIF.

*   Lo concatenamos al resultado:

      IF lv_anyadir = abap_true." INSR JZA Si es multitabla, eliminar los registros en blanco.
*        rv_html_string = |{ rv_html_string }{ lv_substring }|. "LINEA ORIGINAL!
* añadimos el acumulado en aux, por si tenemos celdas en blanco.
        lv_substring_aux = |{ lv_substring_aux }{ lv_substring }|.
        rv_html_string = |{ rv_html_string }{ lv_substring_aux }|.
        CLEAR lv_substring_aux.
      ELSE.
* Acumulamos por si en alguna celda posterior tiene valor
        lv_substring_aux = |{ lv_substring_aux }{ lv_substring }|..
      ENDIF.


*   Se ha acabado?
      IF lv_pos_found LT 0.
        EXIT. " finish!!
      ENDIF.

*   Listo para siguiente iteración:
      lv_end_len = lv_start_off - lv_pos_found + lv_end_len - lv_token_length.
      lv_start_off = lv_pos_found + lv_token_length.
    ENDWHILE.
  ENDMETHOD.


  METHOD constructor.
    mv_langu = iv_langu.


  ENDMETHOD.


  METHOD convert_body_2_bcs.
    CLEAR rt_body.

    " Se adapta la tabla con el cuerpo al formato de la tabla internal del BCS
    LOOP AT mt_body ASSIGNING FIELD-SYMBOL(<ls_body>).
      DATA(lt_mail_tmp) = cl_bcs_convert=>string_to_soli( <ls_body> ).
      INSERT LINES OF lt_mail_tmp INTO TABLE rt_body.
      CLEAR lt_mail_tmp.
    ENDLOOP.

  ENDMETHOD.


  METHOD convert_subject_2_bcs.

    rv_subject = mv_subject.

  ENDMETHOD.


  METHOD generate_dynamic_symbol_table.
    FIELD-SYMBOLS <ls_table_line> TYPE any.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.

    FREE: eo_data_struct, eo_table_data.

    TRY.
        " Creamos una tabla interna  de simbolos pero quitando los carácteres especiales

        DATA(lt_symbols) = VALUE zca_i_mail_template_symbols( FOR <wa> IN it_symbols ( symbol = remove_special_chars( <wa>-symbol )
                                                                                       value = <wa>-value
                                                                                       values_table = <wa>-values_table ) ).

        " Paso 1: generamos la tabla de componentes:
        DATA(lt_components) = VALUE cl_abap_structdescr=>component_table( FOR <wa> IN lt_symbols ( name = <wa>-symbol
                                                                                                   type = cl_abap_elemdescr=>get_string( ) ) ).

        " Paso 2 creamnos la estructura y la instanciamos:
        DATA(lo_structdescr) = cl_abap_structdescr=>create( p_components = lt_components ).    " Component Table
        CREATE DATA eo_data_struct TYPE HANDLE lo_structdescr.
        ASSIGN eo_data_struct->* TO <ls_table_line>.

        " Paso 3: rellenamos la estructura:
        LOOP AT lt_symbols ASSIGNING FIELD-SYMBOL(<ls_symbol>).
          ASSIGN COMPONENT <ls_symbol>-symbol OF STRUCTURE <ls_table_line> TO FIELD-SYMBOL(<lv_comp>).
          IF sy-subrc EQ 0.
            <lv_comp> = <ls_symbol>-value.
          ENDIF.
        ENDLOOP.

        " Paso 4: Creamos la tabla interna
        DATA(lo_tabledescr) = cl_abap_tabledescr=>create( p_line_type = lo_structdescr ).
        CREATE DATA eo_table_data TYPE HANDLE lo_tabledescr.
        ASSIGN eo_table_data->* TO <lt_table>.

        " Paso 5: rellenamos la tabla interna:

        LOOP AT it_symbols_in_table ASSIGNING FIELD-SYMBOL(<ls_symbols_in_table>).
          APPEND INITIAL LINE TO <lt_table> ASSIGNING <ls_table_line>.

          "   Buscamos los campos que vamos a informar para esta clave:
          LOOP AT lt_symbols ASSIGNING <ls_symbol>.


            READ TABLE <ls_symbol>-values_table ASSIGNING FIELD-SYMBOL(<ls_tabla_valores>)
              WITH KEY key = <ls_symbols_in_table>-key.
            IF sy-subrc EQ 0.
              ASSIGN COMPONENT <ls_symbol>-symbol OF STRUCTURE <ls_table_line> TO <lv_comp>.
              IF sy-subrc EQ 0.
                <lv_comp> = <ls_tabla_valores>-value.
              ENDIF.
            ENDIF.
          ENDLOOP.


        ENDLOOP.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD get_internal_mail_id.
    DATA:
      lv_string_mime TYPE string,
      BEGIN OF ls_bcsd_envid,
        mandt      TYPE bcsd_envid-mandt,
        envid      TYPE bcsd_envid-envid,
        message_id TYPE bcsd_envid-message_id,
      END OF ls_bcsd_envid,
      lv_request_id TYPE bcsd_envid-request_id,
      lv_domain     TYPE sx_domain.

*           Esperamos unos segundos:
    DO 10 TIMES.

      TRY.

          IF mo_mail IS BOUND.

            DATA(oid) = mo_mail->oid( ).

            DATA(lo_mail) = cl_bcs=>get_instance_by_oid( oid ).
            lo_mail->send_request->as_mime_message( EXPORTING do_not_create = abap_true
                                                   IMPORTING mime_message  = DATA(lt_mime) ).

            CALL FUNCTION 'CRM_IC_XML_XSTRING2STRING'
              EXPORTING
                inxstring = lt_mime
              IMPORTING
                outstring = lv_string_mime.

            ev_internal_mail_id = substring_after(  val = lv_string_mime      sub  = |Message-ID: | ).
            ev_internal_mail_id = substring_before( val = ev_internal_mail_id sub = cl_abap_char_utilities=>cr_lf ).

            IF ev_internal_mail_id IS INITIAL.
*                 Si no se encuentra, se intentara generar manualmente:
              lv_request_id = oid.
              SELECT mandt envid message_id                                                              "$sst: #601
                INTO ls_bcsd_envid
                FROM bcsd_envid UP TO 1 ROWS                                                             "$sst: #601
               WHERE request_id EQ oid
               ORDER BY PRIMARY KEY.                                                                     "$sst: #601
              ENDSELECT.                                                                                 "$sst: #601
              IF ls_bcsd_envid-message_id IS NOT INITIAL.                                                "$sst: #900
                CALL FUNCTION 'SX_DEFAULT_INTERNET_DOMAIN_GET'
                  IMPORTING
                    domain             = lv_domain
                  EXCEPTIONS
                    err_domain_not_set = 1
                    err_internal       = 2
                    OTHERS             = 3.

                ev_internal_mail_id = |<{ ls_bcsd_envid-message_id }{ ls_bcsd_envid-mandt }{ ls_bcsd_envid-envid }@{ lv_domain }>|.
                "ELSE.                                                                                  "$sst: #900

              ENDIF.
            ENDIF.
          ENDIF.

        CATCH cx_root.
      ENDTRY.

      IF ev_internal_mail_id IS NOT INITIAL.
        EXIT.
      ENDIF.
      WAIT UP TO 1 SECONDS.

    ENDDO.

    IF ev_internal_mail_id IS INITIAL.
      ev_internal_mail_id = oid.
    ENDIF.
  ENDMETHOD.


  METHOD remove_html_comment.
    DATA: lv_pos_in              TYPE i,
          lv_pos_end             TYPE i,
          lv_start_symbol_lenght TYPE i.

    CLEAR: rv_out_string.

    lv_start_symbol_lenght = strlen( zif_ca_mail_data=>cs_mail-symbols-start_symbol ).

    lv_pos_in  = find( val   = iv_in_string
                       sub   = zif_ca_mail_data=>cs_mail-symbols-start_symbol
                       case  = abap_false ).

    lv_pos_end = find( val   = iv_in_string
                       sub   = zif_ca_mail_data=>cs_mail-symbols-end_symbol
                       case  = abap_false ).

    IF lv_pos_in GE 0 AND lv_pos_end GE lv_start_symbol_lenght.

      lv_pos_in  = lv_pos_in  + lv_start_symbol_lenght.
      lv_pos_end = lv_pos_end - lv_pos_in.

      rv_out_string = substring( val   = iv_in_string
                                 off   = lv_pos_in
                                 len   = lv_pos_end ).
    ENDIF.
  ENDMETHOD.


  METHOD remove_special_chars.
    DATA:
      lv_allowed_char    TYPE char27,                       "#EC *
      lv_replace_by_a(5) TYPE c VALUE 'ÄÀÂÃÁ',              "#EC *
      lv_replace_by_e(4) TYPE c VALUE 'ËÈÊÉ',               "#EC *
      lv_replace_by_i(4) TYPE c VALUE 'ÏÌÎÍ',               "#EC *
      lv_replace_by_o(5) TYPE c VALUE 'ÖÒÔÕÓ',              "#EC *
      lv_replace_by_u(4) TYPE c VALUE 'ÜÙÛÚ'.               "#EC *

    DATA:
      lv_string(1000) TYPE c,
      lv_length       TYPE i,
      lv_index        TYPE i.

    CONSTANTS lc_numbers TYPE char10 VALUE '0123456789'.

    lv_allowed_char = sy-abcde && '_'.
    lv_string = iv_symbol.

    TRANSLATE lv_string TO UPPER CASE.

    lv_length = strlen( lv_string ).

    IF lv_string(1) CA lc_numbers.
      lv_string(1) = space.
    ENDIF.

    DO lv_length TIMES.
      lv_index = sy-index - 1.

      IF ( lv_string+lv_index(1) CA lv_allowed_char OR
           lv_string+lv_index(1) CA lc_numbers ).

        CONTINUE.
      ENDIF.

      IF lv_string+lv_index(1) CA lv_replace_by_a.
        lv_string+lv_index(1) = 'A'.
      ELSEIF lv_string+lv_index(1) CA lv_replace_by_e.
        lv_string+lv_index(1) = 'E'.
      ELSEIF lv_string+lv_index(1) CA lv_replace_by_i.
        lv_string+lv_index(1) = 'I'.
      ELSEIF lv_string+lv_index(1) CA lv_replace_by_o.
        lv_string+lv_index(1) = 'O'.
      ELSEIF lv_string+lv_index(1) CA lv_replace_by_u.
        lv_string+lv_index(1) = 'U'.
      ELSE.
* character cannot be mapped to an allowed character or is not
* in the list of allowed characters. It will be set to blank.
        lv_string+lv_index(1) = ' '.
      ENDIF.
    ENDDO.

    CONDENSE lv_string NO-GAPS.
    rv_symbol = lv_string.
  ENDMETHOD.


  METHOD replace_symbols_body_subject.

    "Se reemplazan los caracteres extraños que añaden los editores HTML
    REPLACE ALL OCCURRENCES OF: zif_ca_mail_data=>cs_mail-symbols-amp_init_symbol IN TABLE mt_body WITH zif_ca_mail_data=>cs_mail-symbols-start_symbol,
                                zif_ca_mail_data=>cs_mail-symbols-amp_end_symbol  IN TABLE mt_body WITH zif_ca_mail_data=>cs_mail-symbols-end_symbol,
                                zif_ca_mail_data=>cs_mail-symbols-amp_init_symbol IN mv_subject WITH zif_ca_mail_data=>cs_mail-symbols-start_symbol,
                                zif_ca_mail_data=>cs_mail-symbols-amp_end_symbol  IN mv_subject WITH zif_ca_mail_data=>cs_mail-symbols-end_symbol.


    " Se sustituye los simbols en la variable del asunto y del cuerpo. En el caso del cuerpo solo se hace si la
    " tabla de simbolos en tabla no esta informada
* Recorro los simbolos para ir reemplazandolos en el asunto y cuerpo.
    LOOP AT it_symbols ASSIGNING FIELD-SYMBOL(<ls_symbol>).
      REPLACE ALL OCCURRENCES OF <ls_symbol>-symbol IN mv_subject WITH <ls_symbol>-value.
      IF it_symbols_in_table IS INITIAL.
        REPLACE ALL OCCURRENCES OF <ls_symbol>-symbol IN TABLE mt_body WITH <ls_symbol>-value.
      ENDIF.
    ENDLOOP.

    "  Nueva rutina para cambiar las claves en el cuerpo para insertar tablas:
    IF it_symbols_in_table IS NOT INITIAL.
      TRY.
          replace_symbols_in_table( EXPORTING it_symbols = it_symbols
                                          it_symbols_in_table     = it_symbols_in_table
                                 CHANGING ct_body      = mt_body ).
        CATCH cx_root.
          es_return = zcl_ca_utilities=>fill_return( iv_type       = zif_ca_mail_data=>cs_message-error
                                                     iv_id         = zif_ca_mail_data=>cs_message-id
                                                     iv_number     = '001' " Error al construir el cuerpo del mail
                                                     iv_langu      = mv_langu ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD replace_symbols_in_table.
    FIELD-SYMBOLS <lt_table_data> TYPE STANDARD TABLE.

    DATA lv_initial_html_string TYPE string.
    DATA lv_result_html_string  TYPE string.
    DATA lo_table_data          TYPE REF TO data.
    DATA lo_symbols            TYPE REF TO data.

    " Se concatena todos el cuerpo en una sola variable
    CONCATENATE LINES OF ct_body INTO lv_initial_html_string RESPECTING BLANKS.

    "  Creamos una tabla interna con los simbolos a insertar, y una estructura con los datos genericos

    " Creamos una tabla interna con los simbolos a insertar, y una estructura con los datos genericos
    generate_dynamic_symbol_table( EXPORTING it_symbols   = it_symbols
                                             it_symbols_in_table       = it_symbols_in_table
                                   IMPORTING eo_table_data  = lo_table_data
                                             eo_data_struct = lo_symbols ).

    ASSIGN lo_table_data->* TO <lt_table_data>.
    ASSIGN lo_symbols->*   TO FIELD-SYMBOL(<ls_simbolos>).

    IF <lt_table_data> IS ASSIGNED AND <ls_simbolos> IS ASSIGNED.
      lv_result_html_string = apply_data_to_template( iv_html_string  = lv_initial_html_string
                                                      it_data         = <lt_table_data>
                                                      is_general_data = <ls_simbolos> ).
    ENDIF.

    SPLIT lv_result_html_string AT cl_abap_char_utilities=>cr_lf INTO TABLE ct_body.

  ENDMETHOD.


  METHOD send.

    CLEAR: es_return, ev_body, ev_internal_mail_id, ev_subject.

    mv_subject = iv_subject. " Se guarda el asunto

    " El cuerpo se parte en línea si tienes carácteres de salto de línea
    SPLIT iv_body AT cl_abap_char_utilities=>cr_lf INTO TABLE mt_body.

    " Se hace lo mismo con la firma, si esta informada.
    SPLIT iv_signature AT cl_abap_char_utilities=>cr_lf INTO TABLE DATA(lt_signature).

    " Se añade la firma al cuerpo
    LOOP AT lt_signature ASSIGNING FIELD-SYMBOL(<ls_signature>).
      INSERT <ls_signature> INTO TABLE mt_body.
    ENDLOOP.

    " En modo previsualización se construye el asunto y cuerpo y se devuelve sus textos. En modo
    " normal se hace el mail pero enviandolo.
    IF iv_preview = abap_false.
      TRY.

          " Instancia de la clase de mail
          mo_mail = cl_bcs=>create_persistent( ).

          " Se añaden los destinatarios del mail
          set_recipientes( EXPORTING it_recipients     = it_recipients
                                     it_recipients_cc  = it_recipients_cc
                                     it_recipients_bcc = it_recipients_bcc
                           IMPORTING es_return         = es_return ).

          " Destinatario
          IF iv_sender IS NOT INITIAL.
            set_sender( iv_sender ).
          ENDIF.

          IF iv_replyto IS NOT INITIAL.
            set_replyto( iv_replyto ).
          ENDIF.

          " Si hay imagenes el cuerpo y asunto se añadirán de manera a distinta que si no hubiera
          IF it_images IS NOT INITIAL.

            "Se añade el cuerpo, el asunto y las imágenes
            set_subject_body_with_mail(
              EXPORTING
                it_images = it_images
                it_symbols = it_symbols
                it_symbols_in_table     = it_symbols_in_table
              IMPORTING
                es_return     = es_return ).


          ELSE.

            set_subject_body( EXPORTING it_symbols = it_symbols
                                          it_symbols_in_table = it_symbols_in_table
                                IMPORTING es_return = es_return ).
          ENDIF.

          IF es_return IS INITIAL. " Si no hay errores se continua

            " Se informan los adjuntos
            set_attachs( EXPORTING it_attachs = it_attachs
                         IMPORTING es_return = es_return ).

            IF es_return IS INITIAL.

              " Se establecen los atributos generales del mail
              set_attributes(  ).

              " Se pasa al mail el asunto, cuerpo e imagenes
              mo_mail->set_document( mo_doc_bcs ).

              " Si he ha marcado el parámetro se informar el asunto del correocon un texto muy largo
              IF iv_set_long_subjet EQ abap_true.
                mo_mail->set_message_subject( mv_subject ).
              ENDIF.

              " Se indica si se quiere confirmación de lectura
              IF iv_request_lecture EQ abap_false.
                mo_mail->set_status_attributes( i_requested_status = zif_ca_mail_data=>cs_mail-send_confirmation-only_error  ).
              ELSE.
                mo_mail->set_status_attributes( i_requested_status = zif_ca_mail_data=>cs_mail-send_confirmation-read  ).
              ENDIF.

              DATA(lv_enviado) = mo_mail->send( i_with_error_screen = 'X' ).

              IF iv_commit = abap_true.
                COMMIT WORK AND WAIT.
              ENDIF.


              " Si se ha enviado y si se pasado el parámetro para saber el número interno se lanza el proceso para obtenerlo
              IF lv_enviado = abap_true AND ev_internal_mail_id IS SUPPLIED.
                get_internal_mail_id( IMPORTING ev_internal_mail_id = ev_internal_mail_id ).
              ENDIF.


            ENDIF.

          ENDIF.


        CATCH cx_root.
          es_return = zcl_ca_utilities=>fill_return( iv_type       = zif_ca_mail_data=>cs_message-error
                                                     iv_id         = zif_ca_mail_data=>cs_message-id
                                                     iv_number     = '002' " Error al enviar mail
                                                     iv_langu      = mv_langu ).
      ENDTRY.

    ELSE.
      set_subject_body( EXPORTING it_symbols = it_symbols
                                    it_symbols_in_table = it_symbols_in_table
                          IMPORTING es_return = es_return ).
    ENDIF.

    " Si se pasada el parámetro del cuerpo ya formateado, se pasa la variable global al parámetro
    IF ev_body IS SUPPLIED.
      CONCATENATE LINES OF mt_body INTO ev_body.
    ENDIF.

    " Lo mismo para el asunto
    IF ev_subject IS SUPPLIED.
      ev_subject = mv_subject.
    ENDIF.

  ENDMETHOD.


  METHOD send_with_template.

    DATA(lo_text_template) = NEW zcl_ca_text_template( ).

    "Comprobar que existe la plantilla
    IF lo_text_template->exist(
                              iv_appl  = iv_appl
                              iv_name  = iv_template
                              iv_langu = iv_langu
                          ) = abap_true.

      "Se lee la plantilla
      lo_text_template->read(
        EXPORTING
          iv_appl  = iv_appl
          iv_name  = iv_template
          iv_langu = iv_langu
        IMPORTING
          et_data  = DATA(lt_data)
      ).

      "Cuerpo
      READ TABLE lt_data ASSIGNING FIELD-SYMBOL(<fs_data>)
      WITH KEY langu = iv_langu
               section = zif_ca_ttemplate_data=>cs_section-body.
      IF sy-subrc EQ 0.
        DATA(lv_body) = <fs_data>-content.
      ENDIF.

      "Asunto
      READ TABLE lt_data ASSIGNING <fs_data>
      WITH KEY langu = iv_langu
               section = zif_ca_ttemplate_data=>cs_section-subject.
      IF sy-subrc EQ 0.
        DATA(lv_subject) = <fs_data>-content.
      ENDIF.

      send(
        EXPORTING
          it_images           = it_images
          it_attachs          = it_attachs
          it_recipients       = it_recipients
          it_recipients_cc    = it_recipients_cc
          it_recipients_bcc   = it_recipients_bcc
          iv_sender           = iv_sender
          it_symbols          = it_symbols
          it_symbols_in_table = it_symbols_in_table
          iv_request_lecture  = iv_request_lecture
          iv_commit           = iv_commit
          iv_replyto          = iv_replyto
          iv_appl             = iv_appl
          iv_set_long_subjet  = iv_set_long_subjet
          iv_body             = lv_body
          iv_subject          = lv_subject
*          iv_signature        =
*          iv_preview          = ABAP_FALSE
        IMPORTING
          es_return           = es_return
*          ev_internal_mail_id =
*          ev_body             =
*          ev_subject          =
      ).


    ELSE.

      es_return-id = zif_ca_mail_data=>cs_message-id.
      es_return-number = 003.
      es_return-type = zif_ca_mail_data=>cs_message-error.
      MESSAGE e003(zca_mail) INTO es_return-message.
*   The template doesn't exists

    ENDIF.

  ENDMETHOD.


  METHOD set_attachs.

    CLEAR: es_return.

    LOOP AT it_attachs ASSIGNING FIELD-SYMBOL(<ls_adjuntos>) WHERE content_bin IS NOT INITIAL.


      DATA(lv_length) = xstrlen( <ls_adjuntos>-content_bin ).
      DATA(lv_so_len) = CONV so_obj_len( lv_length ).
      DATA(lt_solix) = cl_bcs_convert=>xstring_to_solix( <ls_adjuntos>-content_bin ).
      TRY.
          CALL METHOD mo_doc_bcs->add_attachment
            EXPORTING
              i_attachment_type    = 'BIN'
              i_attachment_subject = <ls_adjuntos>-name
              i_att_content_hex    = lt_solix
              i_attachment_size    = lv_so_len.
        CATCH cx_root.
          es_return = zcl_ca_utilities=>fill_return( iv_type       = zif_ca_mail_data=>cs_message-error
                                                       iv_id         = zif_ca_mail_data=>cs_message-id
                                                       iv_number     = '002' " Error al enviar mail
                                                       iv_langu      = mv_langu ).
      ENDTRY.
      CLEAR: lt_solix, lv_length.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_attributes.
    DATA lv_utc TYPE timestamp.
    DATA lv_zone TYPE tznzone VALUE 'CET'.

    TRY.
* Fecha de caducidad. Se ha de pasar a timestamp para pasarlo a la clase.
*      CONVERT DATE et_doc_data-expiry_dat INTO TIME STAMP ld_utc TIME ZONE ld_zone.
*      mo_mail->set_expires_on( ld_utc ).

        " Envio inmediato
        mo_mail->set_send_immediately( abap_true ).

        " Que se guarde la informe MIME, para poder reprocesar mail si hay imagenes.
        mo_mail->set_keep_mime( abap_true ).

      CATCH cx_bcs.
    ENDTRY.
  ENDMETHOD.


  METHOD set_images.
    DATA: lo_mime_api     TYPE REF TO if_mr_api,
          ls_folder       TYPE boole_d,
          lv_content      TYPE xstring,
          lv_xstring      TYPE xstring,
          lv_loio         TYPE skwf_io,
          lv_offset       TYPE so_obj_len,
          lv_dif          TYPE so_obj_len,
          lv_length_255   TYPE so_obj_len,
          lt_solix        TYPE solix_tab,
          ls_solix        TYPE solix,
          lv_content_type TYPE w3conttype,
          lv_filename     TYPE mime_text,
          lv_length       TYPE so_obj_len,
          lv_content_id   TYPE mime_cntid.


    lo_mime_api = cl_mime_repository_api=>if_mr_api~get_api( ).
    CREATE OBJECT mo_mime_helper.

    "Se añaden las imágenes
    LOOP AT it_images ASSIGNING FIELD-SYMBOL(<fs_img>).

      CALL METHOD lo_mime_api->get
        EXPORTING
          i_url              = <fs_img>-url
        IMPORTING
          e_is_folder        = ls_folder
          e_content          = lv_content
          e_loio             = lv_loio
        EXCEPTIONS
          parameter_missing  = 1
          error_occured      = 2
          not_found          = 3
          permission_failure = 4
          OTHERS             = 5.

      "Se trata la imagen para añadirla al mail
      CLEAR: lv_length, lv_xstring.
      lv_length = xstrlen( lv_content ).
      lv_xstring = lv_content(lv_length).

      lv_offset = 0.
      lv_length_255 = 255.

      REFRESH lt_solix.
      WHILE lv_offset < lv_length.
        lv_dif = lv_length - lv_offset.

        CLEAR ls_solix.
        IF lv_dif > lv_length_255.
          ls_solix-line = lv_xstring+lv_offset(lv_length_255).
        ELSE.
          ls_solix-line = lv_xstring+lv_offset(lv_dif).
        ENDIF.

        APPEND ls_solix TO lt_solix.
        ADD lv_length_255 TO lv_offset.
      ENDWHILE.

      CLEAR: lv_content_type, lv_content_id, lv_filename.
      lv_content_type = <fs_img>-mimetype.
      lv_filename = <fs_img>-image_id.
      lv_content_id = <fs_img>-image_id.

      mo_mime_helper->add_binary_part(
        EXPORTING
          content      = lt_solix     " Objcont and Objhead as Table Type
          filename     = lv_filename    " File Name (Proposal Only)
          extension    = 'JPG'    " File extension for PC application
          content_type = lv_content_type    " HTML content type
          length       = lv_length    " Size of Document Content
          content_id   = lv_content_id    " BCOM: Bodypart Content ID
      ).

    ENDLOOP.
  ENDMETHOD.


  METHOD set_recipientes.



    IF it_recipients IS NOT INITIAL.

      LOOP AT it_recipients  ASSIGNING FIELD-SYMBOL(<ls_receivers>).
        TRY.
            mo_mail->add_recipient( i_recipient = cl_cam_address_bcs=>create_internet_address( <ls_receivers> )
                                    i_express = abap_true ).

          CATCH cx_root .
        ENDTRY.
      ENDLOOP.
    ENDIF.

    IF it_recipients_cc IS NOT INITIAL.

      LOOP AT it_recipients_cc  ASSIGNING <ls_receivers>.
        TRY.
            mo_mail->add_recipient( i_recipient = cl_cam_address_bcs=>create_internet_address( <ls_receivers> )
                                    i_copy = abap_true
                                    i_express = abap_true ).

          CATCH cx_root .
        ENDTRY.
      ENDLOOP.
    ENDIF.

    IF it_recipients_bcc IS NOT INITIAL.

      LOOP AT it_recipients_bcc  ASSIGNING <ls_receivers>.
        TRY.
            mo_mail->add_recipient( i_recipient = cl_cam_address_bcs=>create_internet_address( <ls_receivers> )
                                    i_blind_copy = abap_true
                                    i_express = abap_true ).

          CATCH cx_root .
        ENDTRY.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD set_replyto.
    TRY.

        mo_mail->set_reply_to( i_reply_to =  cl_cam_address_bcs=>create_internet_address( iv_replyto ) ).

      CATCH cx_root .
    ENDTRY.
  ENDMETHOD.


  METHOD set_sender.

    TRY.

        mo_mail->set_sender( i_sender =  cl_cam_address_bcs=>create_internet_address( iv_sender ) ).

      CATCH cx_root .
    ENDTRY.
  ENDMETHOD.


  METHOD set_structure_symbols.

    DATA lo_str TYPE REF TO cl_abap_structdescr.
    DATA lv_value_aux  TYPE c LENGTH 255.
    DATA lv_value TYPE string.

    "Se obtienen los componentes de la estrutura
    lo_str ?= cl_abap_typedescr=>describe_by_data( is_structure ).
    DATA(lt_comp) = lo_str->get_ddic_field_list( p_including_substructres = abap_true ).

    "Por cada campo de la estrutura se añade su símbolo
    LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<fs_field>).

      ASSIGN COMPONENT <fs_field>-fieldname OF STRUCTURE is_structure TO FIELD-SYMBOL(<fs_value>).
      IF sy-subrc EQ 0
     AND <fs_value> IS NOT INITIAL.

        CASE <fs_field>-inttype.
          WHEN zif_ca_mail_data=>cs_datatypes-inttype-date.
            WRITE <fs_value> TO lv_value_aux.
            lv_value = lv_value_aux.
          WHEN OTHERS.

            IF <fs_field>-convexit IS NOT INITIAL.

              DATA(lv_conv) = |CONVERSION_EXIT_{ <fs_field>-convexit CASE = UPPER }_OUTPUT|.

              CALL FUNCTION lv_conv
                EXPORTING
                  input  = <fs_value>
                IMPORTING
                  output = lv_value.

              CLEAR lv_conv.
            ELSE.
              lv_value = |{ <fs_value> }|.
            ENDIF.
        ENDCASE.

        set_symbols_mail(
          EXPORTING
            iv_name         = |{ <fs_field>-fieldname CASE = UPPER }|    " String Type
            iv_value        = lv_value    " String Type
          CHANGING
            ct_symbols_mail = ct_symbols_mail    " CA - Mail tempalte symbols
        ).

        CLEAR: lv_value, lv_value_aux.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_subject_body.

    CLEAR: es_return.


    " Se reemplaza los simbolos en el cuerpo y asunto.
    replace_symbols_body_subject( EXPORTING it_symbols = it_symbols
                                            it_symbols_in_table = it_symbols_in_table
                                  IMPORTING es_return = es_return ).


    IF es_return IS INITIAL. " Sin errores se continua el proceso

      TRY.
          mo_doc_bcs = cl_document_bcs=>create_document( i_type = zif_ca_mail_data=>cs_mail-type-html
                                                         i_text = convert_body_2_bcs( )
                                                         i_subject = convert_subject_2_bcs(  ) ).
        CATCH cx_root.

          es_return = zcl_ca_utilities=>fill_return( iv_type       = zif_ca_mail_data=>cs_message-error
                                                     iv_id         = zif_ca_mail_data=>cs_message-id
                                                     iv_number     = '002' " Error al enviar mail
                                                     iv_langu      = mv_langu ).
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD set_subject_body_with_mail.

    " Las imagenes se guarda en un objeto MIME para luego añadir el cuerpo del mensaje
    set_images( it_images ).

    " Se reemplaza los simbolos en el cuerpo y asunto.
    replace_symbols_body_subject( EXPORTING it_symbols = it_symbols
                                            it_symbols_in_table = it_symbols_in_table
                                  IMPORTING es_return = es_return ).
    IF es_return IS INITIAL.

      " Se pasa el cuerpo al MIME
      CALL METHOD mo_mime_helper->set_main_html
        EXPORTING
          content = convert_body_2_bcs( ).

      " Se crea el objecto BCS pasando el cuerpo + el asunto
      TRY.

          mo_doc_bcs = cl_document_bcs=>create_from_multirelated(
              i_subject          = convert_subject_2_bcs( )
              i_multirel_service = mo_mime_helper ).

        CATCH cx_root.

          es_return = zcl_ca_utilities=>fill_return( iv_type       = zif_ca_mail_data=>cs_message-error
                                                     iv_id         = zif_ca_mail_data=>cs_message-id
                                                     iv_number     = '002' " Error al enviar mail
                                                     iv_langu      = mv_langu ).
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD set_symbols_mail.

    DATA: lv_temp_symbol TYPE bsstring.

    FIELD-SYMBOLS: <fs_symbol_mail> LIKE LINE OF ct_symbols_mail,
                   <fs_value>       TYPE zca_s_mail_table_symbols_value.

    IF iv_name IS NOT INITIAL.

      "Se mapea el nombre del símbolo a incluir
      lv_temp_symbol = |{ zif_ca_mail_data=>cs_mail-symbols-start_symbol }{ iv_name }{ zif_ca_mail_data=>cs_mail-symbols-end_symbol }|.

      CASE iv_table.
        WHEN abap_true.

          READ TABLE ct_symbols_mail ASSIGNING <fs_symbol_mail>
          WITH KEY symbol = lv_temp_symbol.

          IF sy-subrc NE 0.
            INSERT INITIAL LINE INTO TABLE ct_symbols_mail ASSIGNING <fs_symbol_mail>.
          ENDIF.

          <fs_symbol_mail>-symbol = lv_temp_symbol.

          INSERT INITIAL LINE INTO TABLE <fs_symbol_mail>-values_table ASSIGNING <fs_value>.

          IF <fs_value> IS ASSIGNED.

            <fs_value>-key   = iv_table_key.
            <fs_value>-value = iv_value.
          ENDIF.

        WHEN abap_false.

          INSERT INITIAL LINE INTO TABLE ct_symbols_mail ASSIGNING <fs_symbol_mail>.

          <fs_symbol_mail>-symbol       = lv_temp_symbol.
          <fs_symbol_mail>-value         = iv_value.

        WHEN OTHERS.
      ENDCASE.

    ENDIF.


  ENDMETHOD.
ENDCLASS.
