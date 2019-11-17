CLASS zcl_ca_send_mail DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.



    METHODS constructor
      IMPORTING
        iv_langu TYPE sylangu DEFAULT sy-langu.

    METHODS send_with_template
      IMPORTING
        !it_attachs          TYPE zca_i_mail_attach OPTIONAL
        !iv_template         TYPE zca_e_mail_template
        !it_recipients       TYPE bcsy_smtpa
        !iv_sender           TYPE ad_smtpadr OPTIONAL
        !it_symbols          TYPE zca_i_mail_template_symbols OPTIONAL
        !iv_request_lecture  TYPE sap_bool DEFAULT abap_false
        !iv_commit           TYPE sap_bool DEFAULT abap_true
        !iv_replyto          TYPE ad_smtpadr OPTIONAL
        !iv_appl             TYPE any OPTIONAL
        !iv_set_long_subjet  TYPE sap_bool DEFAULT abap_false
      EXPORTING
        !es_return           TYPE bapiret2
        !ev_internal_mail_id TYPE char100 .

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
        !ev_internal_mail_id TYPE char100 .

  PROTECTED SECTION.

    TYPES: tt_body TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DATA mo_mail TYPE REF TO cl_bcs .
    DATA mo_doc_bcs TYPE REF TO cl_document_bcs .
    DATA mv_subject TYPE string .
    DATA mt_body TYPE tt_body .
    DATA mv_langu TYPE sylangu .
*  data mO_PLANTILLA type ref to ZCL_CA_PLANTILLA_MAIL .
    DATA mo_mime_helper TYPE REF TO cl_gbt_multirelated_service .
    METHODS build_subject_body
      IMPORTING
        it_symbols          TYPE zca_i_mail_template_symbols OPTIONAL
        it_symbols_in_table TYPE zca_i_mail_table_symbols_value OPTIONAL
      EXPORTING
        es_return           TYPE bapiret2.
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

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_ca_send_mail IMPLEMENTATION.
  METHOD send_with_template.
  ENDMETHOD.

  METHOD send.

    CLEAR: es_return.

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

        CATCH cx_root.
          es_return = zcl_ca_utilities=>fill_return( iv_type       = zif_ca_mail_data=>cs_message-error
                                                     iv_id         = zif_ca_mail_data=>cs_message-id
                                                     iv_number     = '002' " Error al enviar mail
                                                     iv_langu      = mv_langu ).
      ENDTRY.

    ELSE.
      build_subject_body( EXPORTING it_symbols = it_symbols
                                    it_symbols_in_table = it_symbols_in_table
                          IMPORTING es_return = es_return ).
    ENDIF.

  ENDMETHOD.

  METHOD constructor.
    mv_langu = iv_langu.


  ENDMETHOD.


  METHOD build_subject_body.
    DATA lt_body_mail TYPE bcsy_text.

    CLEAR: es_return.

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

    IF es_return IS INITIAL. " Sin errores se continua el proceso

      " Se adapta la tabla con el cuerpo al formato de la tabla internal del BCS
      LOOP AT mt_body ASSIGNING FIELD-SYMBOL(<ls_body>).
        DATA(lt_mail_tmp) = cl_bcs_convert=>string_to_soli( <ls_body> ).
        INSERT LINES OF lt_mail_tmp INTO TABLE lt_body_mail.
        CLEAR lt_mail_tmp.
      ENDLOOP.

      " Se pasa el asunto
      DATA(lv_subject_mail) = CONV so_obj_des( mv_subject ).

      TRY.
          mo_doc_bcs = cl_document_bcs=>create_document( i_type = zif_ca_mail_data=>cs_mail-type-html
                                                         i_text = lt_body_mail
                                                         i_subject = lv_subject_mail ).
        CATCH cx_root.

          es_return = zcl_ca_utilities=>fill_return( iv_type       = zif_ca_mail_data=>cs_message-error
                                                     iv_id         = zif_ca_mail_data=>cs_message-id
                                                     iv_number     = '002' " Error al enviar mail
                                                     iv_langu      = mv_langu ).
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD replace_symbols_in_table.
    FIELD-SYMBOLS <lt_table_data> TYPE INDEX TABLE.

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
                                   IMPORTING eo_table_data  = lo_symbols
                                             eo_data_struct = lo_table_data ).

    ASSIGN: lo_table_data->* TO <lt_table_data>,
            lo_symbols->*   TO FIELD-SYMBOL(<ls_simbolos>).

    IF <lt_table_data> IS ASSIGNED AND <ls_simbolos> IS ASSIGNED.
      lv_result_html_string = apply_data_to_template( iv_html_string  = lv_initial_html_string
                                                      it_data         = <lt_table_data>
                                                      is_general_data = <ls_simbolos> ).
    ENDIF.

    SPLIT lv_result_html_string AT cl_abap_char_utilities=>cr_lf INTO TABLE ct_body.

  ENDMETHOD.


  METHOD generate_dynamic_symbol_table.
    FIELD-SYMBOLS <ls_table_line> TYPE any.
    FIELD-SYMBOLS <lt_table> TYPE INDEX TABLE.

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
          LOOP AT it_symbols ASSIGNING <ls_symbol>.


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

ENDCLASS.
