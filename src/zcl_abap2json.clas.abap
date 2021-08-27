class ZCL_ABAP2JSON definition
  public
  create public .

public section.

  types:
    BEGIN OF ts_log,
        table TYPE tabname,
        file  TYPE string,
        text  TYPE string,
        count TYPE i,
      END OF ts_log .
  types:
    tt_log TYPE TABLE OF ts_log .

  constants GC_SPLIT_SIZE type I value 10000 ##NO_TEXT.
  constants GC_ENCODING type ABAP_ENCOD value 'UTF-8' ##NO_TEXT.
  class-data GT_TMS type STMS_WBO_CLIENTS read-only .
  class-data GT_SERVER_INFO type VRM_VALUES read-only .

  class-methods ABAP2JSON
    importing
      !IT_DATA type DATA
      !IV_WHERE type STRING optional
    exporting
      !EV_JSON type STRING
      !EV_JSON_ZIP type XSTRING .
  class-methods JSON2ABAP
    importing
      !IV_JSON_ZIP type XSTRING
    exporting
      !ET_DATA type DATA
      !EV_WHERE type STRING .
  class-methods IMPORT_JSON_ZIP
    importing
      !IV_FOLDER type CLIKE optional
      !IV_DEL type FLAG optional
      !IV_SIMULATE type FLAG optional
      !IV_SHOW_PROGRESS_BAR type FLAG optional
      !IV_SHOW_CONFIRM type FLAG optional
      !IV_RFCDEST type RFCDEST optional
      !IV_CLIENT type MANDT optional
    exporting
      !ET_LOG type TT_LOG
      !EV_ERROR_TEXT type CHAR255 .
  class-methods COMPARE_JSON_ZIP
    importing
      !IV_FOLDER type CLIKE optional
      !IV_SHOW_PROGRESS_BAR type FLAG optional
      !IV_RFCDEST type RFCDEST optional
      !IV_CLIENT type MANDT optional
    exporting
      !ET_LOG type TT_LOG
      !EV_ERROR_TEXT type CHAR255 .
  class-methods EXPORT_TABLE
    importing
      !IV_TABLE type TABNAME
      !IV_WHERE type STRING optional
      !IV_FOLDER type CLIKE optional
      !IV_SHOW_PROGRESS_BAR type FLAG optional
      !IV_RFCDEST type RFCDEST optional
      !IV_CLIENT type MANDT optional
    exporting
      !ET_LOG type TT_LOG
      !EV_ERROR_TEXT type CHAR255 .
  class-methods EXPORT_PACKAGE
    importing
      !IV_PACKAGE type DEVCLASS
      !IV_FOLDER type CLIKE optional
      !IV_SHOW_PROGRESS_BAR type FLAG optional
      !IV_RFCDEST type RFCDEST optional
      !IV_CLIENT type MANDT optional
    exporting
      !ET_LOG type TT_LOG
      !EV_ERROR_TEXT type CHAR255 .
  class-methods COMPARE_TABLE
    importing
      !IV_TABLE type TABNAME
      !IV_WHERE type STRING optional
      !IV_SHOW_PROGRESS_BAR type FLAG optional
      !IV_RFCDEST type RFCDEST optional
      !IV_CLIENT type MANDT optional
      !IV_RFCDEST2 type RFCDEST optional
      !IV_CLIENT2 type MANDT optional
    exporting
      !EV_SAME type FLAG
      !EV_ERROR_TEXT type CHAR255 .
  class-methods COPY_TABLE
    importing
      !IV_TABLE type TABNAME
      !IV_WHERE type STRING optional
      !IV_DEL type FLAG optional
      !IV_SIMULATE type FLAG optional
      !IV_SHOW_PROGRESS_BAR type FLAG optional
      !IV_RFCDEST type RFCDEST optional
      !IV_CLIENT type MANDT optional
      !IV_RFCDEST2 type RFCDEST optional
      !IV_CLIENT2 type MANDT optional
    exporting
      !ET_LOG type TT_LOG
      !EV_ERROR_TEXT type CHAR255 .
  class-methods INIT_SERVER_INFO
    returning
      value(RT_DD) type VRM_VALUES .
  class-methods GET_SERVER_INFO
    importing
      !IV_SERVER type CLIKE
    exporting
      !EV_RFCDEST type RFCDEST
      !EV_CLIENT type MANDT .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS get_rfc_error_text
      IMPORTING
        !iv_table            TYPE clike OPTIONAL
      RETURNING
        VALUE(rv_error_text) TYPE text255 .
    CLASS-METHODS get_table_content
      IMPORTING
        !iv_table      TYPE tabname
        !iv_where      TYPE string OPTIONAL
        !iv_rfcdest    TYPE rfcdest OPTIONAL
        !iv_client     TYPE mandt OPTIONAL
        !iv_from       TYPE i OPTIONAL
        !iv_upto       TYPE i OPTIONAL
      EXPORTING
        !et_data       TYPE table
        !ev_error_text TYPE char255 .
    CLASS-METHODS get_http_response
      IMPORTING
        !iv_url            TYPE string
      RETURNING
        VALUE(ro_response) TYPE REF TO if_http_response
      RAISING
        cx_demo_exception .
    CLASS-METHODS get_json_zip_file_list
      IMPORTING
        !iv_folder         TYPE string
        !iv_sub_folder     TYPE flag DEFAULT abap_true
      RETURNING
        VALUE(rt_filename) TYPE stringtab
      RAISING
        cx_demo_exception .
    CLASS-METHODS get_table_name
      IMPORTING
        !iv_filename    TYPE clike
      RETURNING
        VALUE(rv_table) TYPE tabname .
    CLASS-METHODS file_upload
      IMPORTING
        !iv_filename   TYPE string
      EXPORTING
        !ev_xstring    TYPE xstring
        !ev_error_text TYPE char255 .
    CLASS-METHODS file_download
      IMPORTING
        !iv_folder     TYPE string
        !iv_filename   TYPE string
        !iv_xstring    TYPE xstring
      EXPORTING
        !ev_error_text TYPE char255 .
ENDCLASS.



CLASS ZCL_ABAP2JSON IMPLEMENTATION.


  METHOD abap2json.
    DATA: lv_xstring TYPE xstring,
          lo_zip     TYPE REF TO cl_abap_zip,
          lv_lines   TYPE i.
    FIELD-SYMBOLS: <lt_data> TYPE ANY TABLE.


    /ui2/cl_json=>serialize(
      EXPORTING
        data             = it_data             " Data to serialize
        compress         = abap_true         " Skip empty elements
*        name             = name             " Object name
*        pretty_name      = pretty_name      " Pretty Print property names
*        type_descr       = type_descr       " Data descriptor
*        assoc_arrays     = assoc_arrays     " Serialize tables with unique keys as associative array
*        ts_as_iso8601    = ts_as_iso8601    " Dump timestamps as string in ISO8601 format
*        expand_includes  = expand_includes  " Expand named includes in structures
*        assoc_arrays_opt = assoc_arrays_opt " Optimize rendering of name value maps
*        numc_as_string   = numc_as_string   " Serialize NUMC fields as strings
*        name_mappings    = name_mappings    " ABAP<->JSON Name Mapping Table
*        conversion_exits = conversion_exits " Use DDIC conversion exits on serialize of values
      RECEIVING
        r_json           = ev_json           " JSON string
    ).


    CHECK: ev_json_zip IS REQUESTED.

    cl_abap_conv_out_ce=>create( encoding = gc_encoding )->convert(
      EXPORTING
        data   = ev_json
      IMPORTING
        buffer = lv_xstring
    ).

    CREATE OBJECT lo_zip.
    ASSIGN it_data TO <lt_data>.
    lv_lines = lines( <lt_data> ).
    lo_zip->add(
      EXPORTING
        name           = lv_lines && '.json'
        content        = lv_xstring
    ).

    IF iv_where IS NOT INITIAL.
      cl_abap_conv_out_ce=>create( encoding = gc_encoding )->convert(
        EXPORTING
          data   = iv_where
        IMPORTING
          buffer = lv_xstring
      ).
      lo_zip->add(
        EXPORTING
          name           = 'SQL_WHERE.txt'
          content        = lv_xstring
      ).
    ENDIF.

    ev_json_zip = lo_zip->save( ).

  ENDMETHOD.


  METHOD compare_table.
    DATA: ltr_data  TYPE REF TO data,
          lv_total  TYPE i,
          lv_total2 TYPE i,
          lv_from   TYPE i,
          lv_index  TYPE i,
          lo_cx     TYPE REF TO cx_demo_exception.
    FIELD-SYMBOLS: <lt_data>  TYPE table,
                   <lt_data2> TYPE table.

    CLEAR: ev_same, ev_error_text.

    IF iv_rfcdest IS NOT INITIAL AND iv_client IS INITIAL.
      ev_error_text = TEXT-e03.
      RETURN.
    ENDIF.
    IF iv_rfcdest2 IS NOT INITIAL AND iv_client2 IS INITIAL.
      ev_error_text = TEXT-e03.
      RETURN.
    ENDIF.
    IF iv_rfcdest EQ iv_rfcdest2 AND iv_client EQ iv_client2.
      ev_error_text = TEXT-e06.
      RETURN.
    ENDIF.

    TRY.

        CALL FUNCTION 'ZA2J_GET'
          DESTINATION iv_rfcdest
          EXPORTING
            iv_client             = iv_client
            iv_tname              = iv_table
            iv_flag_count_only    = abap_true
            iv_where              = iv_where
          IMPORTING
            ev_count              = lv_total
          EXCEPTIONS
            system_failure        = 101 MESSAGE ev_error_text
            communication_failure = 102 MESSAGE ev_error_text
            table_name_error      = 5
            sql_error             = 1
            unkown_error          = 2
            OTHERS                = 3.
        IF sy-subrc <> 0.
          IF ev_error_text IS INITIAL.
            ev_error_text = get_rfc_error_text( iv_table ).
          ENDIF.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
        ENDIF.

        CALL FUNCTION 'ZA2J_GET'
          DESTINATION iv_rfcdest2
          EXPORTING
            iv_client             = iv_client2
            iv_tname              = iv_table
            iv_flag_count_only    = abap_true
            iv_where              = iv_where
          IMPORTING
            ev_count              = lv_total2
          EXCEPTIONS
            system_failure        = 101 MESSAGE ev_error_text
            communication_failure = 102 MESSAGE ev_error_text
            table_name_error      = 5
            sql_error             = 1
            unkown_error          = 2
            OTHERS                = 3.
        IF sy-subrc <> 0.
          IF ev_error_text IS INITIAL.
            ev_error_text = get_rfc_error_text( iv_table ).
          ENDIF.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
        ENDIF.

        IF lv_total <> lv_total2.
          RETURN.
        ENDIF.


        CREATE DATA ltr_data TYPE TABLE OF (iv_table).
        ASSIGN ltr_data->* TO <lt_data>.
        CREATE DATA ltr_data TYPE TABLE OF (iv_table).
        ASSIGN ltr_data->* TO <lt_data2>.

        lv_from = 1.
        WHILE lv_from <= lv_total.

          IF iv_show_progress_bar EQ abap_true.
            CALL FUNCTION 'PROGRESS_INDICATOR'
              EXPORTING
                i_text               = |{ CONV f( 100 * lv_from / lv_total ) DECIMALS = 2 }% of { lv_total NUMBER = USER }|
                i_output_immediately = abap_true
                i_processed          = lv_from
                i_total              = lv_total.
          ENDIF.

          get_table_content(
            EXPORTING
              iv_table      = iv_table
              iv_where      = iv_where
              iv_rfcdest    = iv_rfcdest
              iv_client     = iv_client
              iv_from       = lv_from
              iv_upto       = gc_split_size
            IMPORTING
              et_data       = <lt_data>
              ev_error_text = ev_error_text
          ).
          IF ev_error_text IS NOT INITIAL.
            RETURN.
          ENDIF.

          get_table_content(
            EXPORTING
              iv_table      = iv_table
              iv_where      = iv_where
              iv_rfcdest    = iv_rfcdest2
              iv_client     = iv_client2
              iv_from       = lv_from
              iv_upto       = gc_split_size
            IMPORTING
              et_data       = <lt_data2>
              ev_error_text = ev_error_text
          ).
          IF ev_error_text IS NOT INITIAL.
            RETURN.
          ENDIF.

          IF <lt_data> <> <lt_data2>.
            RETURN.
          ENDIF.

          lv_from = lv_from + gc_split_size.
        ENDWHILE.


        ev_same = abap_true.

      CATCH cx_demo_exception INTO lo_cx.
        ev_error_text = lo_cx->exception_text.
    ENDTRY.

  ENDMETHOD.


  METHOD copy_table.
    DATA: lv_xstring  TYPE xstring,
          lv_xstring2 TYPE xstring,
          ltr_data    TYPE REF TO data,
          lv_total    TYPE i,
          lv_total2   TYPE i,
          lv_from     TYPE i,
          lv_count    TYPE i,
          lv_count2   TYPE i,
          lv_index    TYPE i,
          lo_cx       TYPE REF TO cx_demo_exception.
    CLEAR: et_log, ev_error_text.

    IF iv_rfcdest IS NOT INITIAL AND iv_client IS INITIAL.
      ev_error_text = TEXT-e03.
      RETURN.
    ENDIF.
    IF iv_rfcdest2 IS NOT INITIAL AND iv_client2 IS INITIAL.
      ev_error_text = TEXT-e03.
      RETURN.
    ENDIF.
    IF iv_rfcdest EQ iv_rfcdest2 AND iv_client EQ iv_client2.
      ev_error_text = TEXT-e06.
      RETURN.
    ENDIF.

    TRY.

        IF iv_del EQ abap_true.
*        DELETE FROM (lv_table).
          CALL FUNCTION 'ZA2J_DELETE'
            DESTINATION iv_rfcdest2
            EXPORTING
              iv_client             = iv_client2
              iv_tname              = iv_table
            IMPORTING
              ev_count              = lv_count
            EXCEPTIONS
              system_failure        = 101 MESSAGE ev_error_text
              communication_failure = 102 MESSAGE ev_error_text
              table_name_error      = 5
              sql_error             = 1
              unkown_error          = 2
              OTHERS                = 3.
          IF sy-subrc <> 0.
            IF ev_error_text IS INITIAL.
              ev_error_text = get_rfc_error_text( iv_table ).
            ENDIF.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.
          APPEND VALUE #( table = iv_table text = 'deleted' count = lv_count ) TO et_log.
        ENDIF.

        CALL FUNCTION 'ZA2J_GET'
          DESTINATION iv_rfcdest
          EXPORTING
            iv_client             = iv_client
            iv_tname              = iv_table
            iv_flag_count_only    = abap_true
            iv_where              = iv_where
          IMPORTING
            ev_count              = lv_total
          EXCEPTIONS
            system_failure        = 101 MESSAGE ev_error_text
            communication_failure = 102 MESSAGE ev_error_text
            table_name_error      = 5
            sql_error             = 1
            unkown_error          = 2
            OTHERS                = 3.
        IF sy-subrc <> 0.
          IF ev_error_text IS INITIAL.
            ev_error_text = get_rfc_error_text( iv_table ).
          ENDIF.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
        ENDIF.


        lv_from = 1.
        WHILE lv_from <= lv_total.
          CLEAR: lv_xstring, lv_count.

          IF iv_show_progress_bar EQ abap_true.
            CALL FUNCTION 'PROGRESS_INDICATOR'
              EXPORTING
                i_text               = |{ CONV f( 100 * lv_from / lv_total ) DECIMALS = 2 }% of { lv_total NUMBER = USER }|
                i_output_immediately = abap_true
                i_processed          = lv_from
                i_total              = lv_total.
          ENDIF.

          CALL FUNCTION 'ZA2J_GET'
            DESTINATION iv_rfcdest
            EXPORTING
              iv_client             = iv_client
              iv_tname              = iv_table
              iv_from               = lv_from
              iv_upto               = gc_split_size
              iv_where              = iv_where
            IMPORTING
              ev_json_zip           = lv_xstring
              ev_count              = lv_count
            EXCEPTIONS
              system_failure        = 101 MESSAGE ev_error_text
              communication_failure = 102 MESSAGE ev_error_text
              table_name_error      = 5
              sql_error             = 1
              unkown_error          = 2
              OTHERS                = 3.
          IF sy-subrc <> 0.
            IF ev_error_text IS INITIAL.
              ev_error_text = get_rfc_error_text( iv_table ).
            ENDIF.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.

          CALL FUNCTION 'ZA2J_MODIFY'
            DESTINATION iv_rfcdest2
            EXPORTING
              iv_client             = iv_client2
              iv_tname              = iv_table
              iv_json_zip           = lv_xstring
            IMPORTING
              ev_count              = lv_count
            EXCEPTIONS
              system_failure        = 101 MESSAGE ev_error_text
              communication_failure = 102 MESSAGE ev_error_text
              table_name_error      = 5
              sql_error             = 1
              unkown_error          = 2
              OTHERS                = 3.
          IF sy-subrc <> 0.
            IF ev_error_text IS INITIAL.
              ev_error_text = get_rfc_error_text( iv_table ).
            ENDIF.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.

          APPEND VALUE #( table = iv_table text = 'copy' count = lv_count ) TO et_log.
        ENDWHILE.



        IF iv_simulate EQ abap_false.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            DESTINATION iv_rfcdest2.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
            DESTINATION iv_rfcdest2.
        ENDIF.

      CATCH cx_demo_exception INTO lo_cx.
        ev_error_text = lo_cx->exception_text.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
          DESTINATION iv_rfcdest2.
    ENDTRY.

  ENDMETHOD.


  METHOD export_package.
    DATA: lv_folder TYPE string,
          lt_table  TYPE TABLE OF tabname,
          lv_table  TYPE tabname,
          lt_log    TYPE tt_log,
          lv_total  TYPE i,
          lv_index  TYPE i,
          lo_cx     TYPE REF TO cx_demo_exception.

    CLEAR: et_log, ev_error_text.

    TRY.

        IF iv_rfcdest IS NOT INITIAL AND iv_client IS INITIAL.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = TEXT-e03.
        ENDIF.

        SELECT tabname
          INTO TABLE lt_table
          FROM info_tabl
          WHERE devclass = iv_package
            AND as4local = 'A'
            AND tabclass = 'TRANSP'.
        CHECK: lt_table IS NOT INITIAL.


        IF iv_folder CP 'http*'.
          ev_error_text = 'export to github is not implemanted yet.'.
          RETURN.
        ELSEIF iv_folder IS NOT INITIAL.
          lv_folder = iv_folder.
        ELSE.
          cl_gui_frontend_services=>directory_browse(
            CHANGING
              selected_folder      = lv_folder
            EXCEPTIONS
              cntl_error           = 1               " Control error
              error_no_gui         = 2               " No GUI available
              not_supported_by_gui = 3               " GUI does not support this
              OTHERS               = 4
          ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_error_text.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.
        ENDIF.
        CHECK: lv_folder IS NOT INITIAL.


        lv_total = lines( lt_table ).
        LOOP AT lt_table INTO lv_table.
          lv_index = sy-tabix.

          IF iv_show_progress_bar EQ abap_true.
            CALL FUNCTION 'PROGRESS_INDICATOR'
              EXPORTING
                i_text               = |{ CONV f( 100 * lv_index / lv_total ) DECIMALS = 2 }% { lv_table }|
                i_output_immediately = abap_true
                i_processed          = lv_index
                i_total              = lv_total.
          ENDIF.

          export_table(
            EXPORTING
              iv_table             = lv_table
              iv_folder            = lv_folder
              iv_show_progress_bar = abap_false
              iv_rfcdest           = iv_rfcdest
              iv_client            = iv_client
            IMPORTING
              et_log               = lt_log
              ev_error_text        = ev_error_text
          ).
          APPEND LINES OF lt_log TO et_log.
          IF ev_error_text IS NOT INITIAL.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.

        ENDLOOP.


      CATCH cx_demo_exception INTO lo_cx.
        ev_error_text = lo_cx->exception_text.
    ENDTRY.

  ENDMETHOD.


  METHOD export_table.
    DATA: lv_folder   TYPE string,
          lv_datetime TYPE string,
          lv_filename TYPE string,
          lv_xstring  TYPE xstring,
          lv_total    TYPE i,
          lv_from     TYPE i,
          lv_part_num TYPE i,
          lv_count    TYPE i,
          lv_index    TYPE i,
          lo_cx       TYPE REF TO cx_demo_exception.

    CLEAR: et_log, ev_error_text.

    TRY.

        IF iv_rfcdest IS NOT INITIAL AND iv_client IS INITIAL.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = TEXT-e03.
        ENDIF.

        CALL FUNCTION 'ZA2J_GET'
          DESTINATION iv_rfcdest
          EXPORTING
            iv_client             = iv_client
            iv_tname              = iv_table
            iv_flag_count_only    = abap_true
            iv_where              = iv_where
          IMPORTING
            ev_count              = lv_total
          EXCEPTIONS
            system_failure        = 101 MESSAGE ev_error_text
            communication_failure = 102 MESSAGE ev_error_text
            table_name_error      = 5
            sql_error             = 1
            unkown_error          = 2
            OTHERS                = 3.
        IF sy-subrc <> 0.
          IF ev_error_text IS INITIAL.
            ev_error_text = get_rfc_error_text( iv_table ).
          ENDIF.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
        ENDIF.

        IF lv_total EQ 0.
          APPEND VALUE #( table = iv_table text = 'empty' count = 0 ) TO et_log.
          RETURN.
        ENDIF.


        IF iv_folder CP 'http*'.
          ev_error_text = 'export to github is not implemanted yet.'.
          RETURN.
        ELSEIF iv_folder IS NOT INITIAL.
          lv_folder = iv_folder.
        ELSE.
          cl_gui_frontend_services=>directory_browse(
            CHANGING
              selected_folder      = lv_folder
            EXCEPTIONS
              cntl_error           = 1               " Control error
              error_no_gui         = 2               " No GUI available
              not_supported_by_gui = 3               " GUI does not support this
              OTHERS               = 4
          ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_error_text.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.
        ENDIF.

        CHECK: lv_folder IS NOT INITIAL.

        lv_datetime = sy-datum && sy-uzeit.
        lv_from = 1.
        WHILE lv_from <= lv_total.
          CLEAR: lv_xstring, lv_count.

          IF iv_show_progress_bar EQ abap_true.
            CALL FUNCTION 'PROGRESS_INDICATOR'
              EXPORTING
                i_text               = |{ CONV f( 100 * lv_from / lv_total ) DECIMALS = 2 }% of { lv_total NUMBER = USER }|
                i_output_immediately = abap_true
                i_processed          = lv_from
                i_total              = lv_total.
          ENDIF.

          CALL FUNCTION 'ZA2J_GET'
            DESTINATION iv_rfcdest
            EXPORTING
              iv_client             = iv_client
              iv_tname              = iv_table
              iv_from               = lv_from
              iv_upto               = gc_split_size
              iv_where              = iv_where
            IMPORTING
              ev_json_zip           = lv_xstring
              ev_count              = lv_count
            EXCEPTIONS
              system_failure        = 101 MESSAGE ev_error_text
              communication_failure = 102 MESSAGE ev_error_text
              table_name_error      = 5
              sql_error             = 1
              unkown_error          = 2
              OTHERS                = 3.
          IF sy-subrc <> 0.
            IF ev_error_text IS INITIAL.
              ev_error_text = get_rfc_error_text( iv_table ).
            ENDIF.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.


          IF lv_count IS INITIAL.
            EXIT.
          ELSE.
            lv_from = lv_from + gc_split_size.
            lv_part_num = lv_part_num + 1.
            IF lv_part_num EQ 1 AND lv_total <= gc_split_size.
              lv_filename = iv_table && '.' && lv_datetime && '.json.zip'.
            ELSE.
              lv_filename = iv_table && '.' && lv_datetime && '.part' && lv_part_num && '.json.zip'.
            ENDIF.

            file_download(
              EXPORTING
                iv_folder     = lv_folder
                iv_filename   = lv_filename
                iv_xstring    = lv_xstring
              IMPORTING
                ev_error_text = ev_error_text
            ).
            IF ev_error_text IS NOT INITIAL.
              RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
            ENDIF.

            APPEND VALUE #( table = iv_table file = lv_filename count = lv_count ) TO et_log.

          ENDIF.
        ENDWHILE.


      CATCH cx_demo_exception INTO lo_cx.
        ev_error_text = lo_cx->exception_text.
    ENDTRY.

  ENDMETHOD.


  METHOD get_rfc_error_text.
    CASE sy-subrc.
      WHEN 5.
        rv_error_text = TEXT-e05.
      WHEN 1.
        rv_error_text = TEXT-e01.
      WHEN OTHERS.
        rv_error_text = TEXT-e02.
    ENDCASE.

    IF iv_table IS NOT INITIAL.
      rv_error_text = rv_error_text && `: ` && iv_table.
    ENDIF.
  ENDMETHOD.


  METHOD get_server_info.
    DATA: lv_sysid TYPE sy-sysid,
          lv_mandt TYPE sy-mandt,
          ls_tms   TYPE stms_wbo_client.

    CLEAR: ev_rfcdest, ev_client.

    SPLIT iv_server AT '/' INTO lv_sysid lv_mandt.

    ev_client = lv_mandt.
    IF lv_sysid IS NOT INITIAL.
      READ TABLE gt_tms INTO ls_tms WITH KEY system-sysnam = lv_sysid.
      ev_rfcdest = ls_tms-system-dessup.
    ENDIF.
  ENDMETHOD.


  METHOD import_json_zip.
    DATA: lv_folder       TYPE string,
          lt_filename     TYPE TABLE OF string,
          lv_filename     TYPE string,
          lv_xstring      TYPE xstring,
          lt_table        TYPE TABLE OF tabname,
          lv_table        TYPE tabname,
          lt_confirmtable TYPE sesf_string_tab,
          lv_count        TYPE i,
          lv_total        TYPE i,
          lv_index        TYPE i,
          lv_answer       TYPE c,
          lo_cx           TYPE REF TO cx_demo_exception.

    CLEAR: et_log, ev_error_text.

    TRY.

        IF iv_rfcdest IS NOT INITIAL AND iv_client IS INITIAL.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = TEXT-e03.
        ENDIF.


        IF iv_folder IS NOT INITIAL.
          lv_folder = iv_folder.
        ELSE.
          cl_gui_frontend_services=>directory_browse(
            CHANGING
              selected_folder      = lv_folder
            EXCEPTIONS
              cntl_error           = 1               " Control error
              error_no_gui         = 2               " No GUI available
              not_supported_by_gui = 3               " GUI does not support this
              OTHERS               = 4
          ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_error_text.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.
        ENDIF.

        CHECK: lv_folder IS NOT INITIAL.
        lt_filename = get_json_zip_file_list( lv_folder ).

        IF lt_filename IS INITIAL.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = TEXT-e04.
        ENDIF.
        SORT lt_filename.

        LOOP AT lt_filename INTO lv_filename.
          APPEND get_table_name( lv_filename ) TO lt_table.
        ENDLOOP.
        SORT lt_table.
        DELETE ADJACENT DUPLICATES FROM lt_table.

        IF iv_show_confirm EQ abap_true.
          MOVE-CORRESPONDING lt_table TO lt_confirmtable.
          CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_TABLE'
            EXPORTING
              titlebar        = 'Confirm'
*             start_column    = 25
*             start_row       = 6
*             end_column      = 90
*             end_row         = 20
              columnname      = 'Table'
            IMPORTING
              answer          = lv_answer
            CHANGING
              ct_displaytable = lt_confirmtable.
          IF lv_answer <> 'J'.
            RETURN.
          ENDIF.
        ENDIF.

        IF iv_del EQ abap_true.
          LOOP AT lt_table INTO lv_table.
*        DELETE FROM (lv_table).
            CALL FUNCTION 'ZA2J_DELETE'
              DESTINATION iv_rfcdest
              EXPORTING
                iv_client             = iv_client
                iv_tname              = lv_table
              IMPORTING
                ev_count              = lv_count
              EXCEPTIONS
                system_failure        = 101 MESSAGE ev_error_text
                communication_failure = 102 MESSAGE ev_error_text
                table_name_error      = 5
                sql_error             = 1
                unkown_error          = 2
                OTHERS                = 3.
            IF sy-subrc <> 0.
              IF ev_error_text IS INITIAL.
                ev_error_text = get_rfc_error_text( lv_table ).
              ENDIF.
              RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
            ENDIF.
            APPEND VALUE #( table = lv_table text = 'deleted' count = lv_count ) TO et_log.
          ENDLOOP.
        ENDIF.

        lv_total = lines( lt_filename ).
        LOOP AT lt_filename INTO lv_filename.
          CLEAR: lv_table.

          lv_index = sy-tabix.

          IF iv_show_progress_bar EQ abap_true.
            CALL FUNCTION 'PROGRESS_INDICATOR'
              EXPORTING
                i_text               = |{ CONV f( 100 * lv_index / lv_total ) DECIMALS = 2 }% { lv_table }|
                i_output_immediately = abap_true
                i_processed          = lv_index
                i_total              = lv_total.
          ENDIF.

          file_upload(
            EXPORTING
              iv_filename  = lv_filename
            IMPORTING
              ev_xstring    = lv_xstring
              ev_error_text = ev_error_text
          ).
          IF ev_error_text IS NOT INITIAL.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.

          lv_table = get_table_name( lv_filename ).

*      CREATE DATA lr_data TYPE TABLE OF (lv_table).
*      ASSIGN lr_data->* TO <lt_data>.
*      json2abap(
*        EXPORTING
*          iv_json_zip = lv_xstring
*        IMPORTING
*          et_data     = <lt_data>
*      ).
*      MODIFY (lv_table) FROM TABLE <lt_data>.
*      lv_count = lines( <lt_data> ).

          CALL FUNCTION 'ZA2J_MODIFY'
            DESTINATION iv_rfcdest
            EXPORTING
              iv_client             = iv_client
              iv_tname              = lv_table
              iv_json_zip           = lv_xstring
            IMPORTING
              ev_count              = lv_count
            EXCEPTIONS
              system_failure        = 101 MESSAGE ev_error_text
              communication_failure = 102 MESSAGE ev_error_text
              table_name_error      = 5
              sql_error             = 1
              unkown_error          = 2
              OTHERS                = 3.
          IF sy-subrc <> 0.
            IF ev_error_text IS INITIAL.
              ev_error_text = get_rfc_error_text( lv_table ).
            ENDIF.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.

          APPEND VALUE #( table = lv_table file = lv_filename count = lv_count ) TO et_log.
        ENDLOOP.


        IF iv_simulate EQ abap_false.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            DESTINATION iv_rfcdest.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
            DESTINATION iv_rfcdest.
        ENDIF.

      CATCH cx_demo_exception INTO lo_cx.
        ev_error_text = lo_cx->exception_text.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
          DESTINATION iv_rfcdest.
    ENDTRY.

  ENDMETHOD.


  METHOD init_server_info.
    DATA: lt_dd   TYPE vrm_values,
          ls_dd   TYPE vrm_value,
          ls_tms  TYPE stms_wbo_client,
          lt_t000 TYPE TABLE OF t000,
          ls_t000 TYPE t000.

    IF gt_server_info IS INITIAL.
      SELECT *
        INTO TABLE lt_t000
        FROM t000.
      SORT lt_t000 BY mandt.
      LOOP AT lt_t000 INTO ls_t000 WHERE mandt > 0.
        IF ls_t000-mandt EQ sy-mandt.
          ls_dd-key = space.
        ELSE.
          ls_dd-key = |/{ ls_t000-mandt }|.
        ENDIF.
        ls_dd-text = |{ sy-sysid }/{ ls_t000-mandt }: { ls_t000-mtext }|.
        APPEND ls_dd TO lt_dd.
      ENDLOOP.

      CALL FUNCTION 'TMS_MGR_READ_CLIENT_TABLE'
        IMPORTING
          es_wbo_clients     = gt_tms
        EXCEPTIONS
          system_is_virtual  = 1
          read_config_failed = 2
          OTHERS             = 3.

      LOOP AT gt_tms INTO ls_tms WHERE system-sysnam <> sy-sysid.
        LOOP AT ls_tms-t000 INTO ls_t000 WHERE mandt > 0.
          ls_dd-key = |{ ls_tms-system-sysnam }/{ ls_t000-mandt }|.
          ls_dd-text = |{ ls_tms-system-sysnam }/{ ls_t000-mandt }: { ls_t000-mtext }|.
          APPEND ls_dd TO lt_dd.
        ENDLOOP.
      ENDLOOP.

      gt_server_info = lt_dd.
    ENDIF.

    rt_dd = gt_server_info.

  ENDMETHOD.


  METHOD json2abap.
    DATA: lv_xstring TYPE xstring,
          lv_json    TYPE string,
          lo_zip     TYPE REF TO cl_abap_zip,
          ls_file    TYPE cl_abap_zip=>t_file,
          lv_index   TYPE i.
    CLEAR: et_data, ev_where.

    CREATE OBJECT lo_zip.
    lo_zip->load(
      EXPORTING
        zip             = iv_json_zip
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2
    ).
*    CHECK: sy-subrc EQ 0.
    IF sy-subrc <> 0 OR lo_zip->files IS INITIAL.
      cl_abap_conv_in_ce=>create( encoding = gc_encoding input = iv_json_zip )->read(
        IMPORTING
          data = lv_json
      ).

      /ui2/cl_json=>deserialize(
        EXPORTING
          json             = lv_json
        CHANGING
          data             = et_data
      ).
      RETURN.
    ENDIF.

    IF ev_where IS REQUESTED.
      lo_zip->get(
        EXPORTING
          name                    = 'SQL_WHERE.txt'
        IMPORTING
          content                 = lv_xstring
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3
      ).
      IF sy-subrc EQ 0.
        cl_abap_conv_in_ce=>create( encoding = gc_encoding input = lv_xstring )->read(
          IMPORTING
            data = ev_where
        ).
      ENDIF.
    ENDIF.

    LOOP AT lo_zip->files INTO ls_file WHERE name CP '*.json'.
      lv_index = sy-tabix.
      EXIT.
    ENDLOOP.
    CHECK: lv_index IS NOT INITIAL.

    lo_zip->get(
      EXPORTING
        index                   = lv_index
      IMPORTING
        content                 = lv_xstring
      EXCEPTIONS
        zip_index_error         = 1
        zip_decompression_error = 2
        OTHERS                  = 3
    ).
    CHECK: sy-subrc EQ 0.

    cl_abap_conv_in_ce=>create( encoding = gc_encoding input = lv_xstring )->read(
      IMPORTING
        data = lv_json
    ).

    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = lv_json
      CHANGING
        data             = et_data
    ).
  ENDMETHOD.


  METHOD compare_json_zip.
    DATA: lv_folder       TYPE string,
          lt_filename     TYPE TABLE OF string,
          lv_filename     TYPE string,
          lv_filename_old TYPE string,
          BEGIN OF ls_file_and_table,
            filename TYPE string,
            table    TYPE string,
            parted   TYPE flag,
          END OF ls_file_and_table,
          lt_file_and_table LIKE TABLE OF ls_file_and_table,
          lv_xstring        TYPE xstring,
          lv_json           TYPE string,
          lv_table          TYPE tabname,
          ltr_data          TYPE REF TO data,
          lv_where          TYPE string,
          lv_diffrent       TYPE flag,
          lv_part_num       TYPE i,
          lv_from           TYPE i,
          lv_count          TYPE i,
          lv_total          TYPE i,
          lv_index          TYPE i,
          lo_cx             TYPE REF TO cx_demo_exception.
    FIELD-SYMBOLS: <lt_data>  TYPE table,
                   <lt_data2> TYPE table.

    CLEAR: et_log, ev_error_text.

    TRY.

        IF iv_rfcdest IS NOT INITIAL AND iv_client IS INITIAL.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = TEXT-e03.
        ENDIF.


        IF iv_folder IS NOT INITIAL.
          lv_folder = iv_folder.
        ELSE.
          cl_gui_frontend_services=>directory_browse(
            CHANGING
              selected_folder      = lv_folder
            EXCEPTIONS
              cntl_error           = 1               " Control error
              error_no_gui         = 2               " No GUI available
              not_supported_by_gui = 3               " GUI does not support this
              OTHERS               = 4
          ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_error_text.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.
        ENDIF.

        CHECK: lv_folder IS NOT INITIAL.

        lt_filename = get_json_zip_file_list( lv_folder ).

        IF lt_filename IS INITIAL.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = TEXT-e04.
        ENDIF.
        SORT lt_filename.

        LOOP AT lt_filename INTO lv_filename.
          CLEAR: ls_file_and_table.
          ls_file_and_table-filename = lv_filename.
          REPLACE REGEX '.part\d*.json.zip' IN ls_file_and_table-filename WITH '.part1.json.zip'.
          IF sy-subrc EQ 0.
            IF ls_file_and_table-filename EQ lv_filename_old.
              CONTINUE.
            ENDIF.
            lv_filename_old = ls_file_and_table-filename.
            ls_file_and_table-parted = abap_true.
          ENDIF.
          ls_file_and_table-table = get_table_name( lv_filename ).
          APPEND ls_file_and_table TO lt_file_and_table.
        ENDLOOP.



        lv_total = lines( lt_file_and_table ).
        LOOP AT lt_file_and_table INTO ls_file_and_table.
          CLEAR: lv_diffrent.

          lv_index = sy-tabix.
          lv_table = ls_file_and_table-table.
          lv_filename = ls_file_and_table-filename.

          IF iv_show_progress_bar EQ abap_true.
            CALL FUNCTION 'PROGRESS_INDICATOR'
              EXPORTING
                i_text               = |{ CONV f( 100 * lv_index / lv_total ) DECIMALS = 2 }% { lv_table }|
                i_output_immediately = abap_true
                i_processed          = lv_index
                i_total              = lv_total.
          ENDIF.

          CREATE DATA ltr_data TYPE TABLE OF (lv_table).
          ASSIGN ltr_data->* TO <lt_data>.
          CREATE DATA ltr_data TYPE TABLE OF (lv_table).
          ASSIGN ltr_data->* TO <lt_data2>.


          file_upload(
            EXPORTING
              iv_filename  = lv_filename
            IMPORTING
              ev_xstring    = lv_xstring
              ev_error_text = ev_error_text
          ).
          IF ev_error_text IS NOT INITIAL.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.

          json2abap(
            EXPORTING
              iv_json_zip = lv_xstring
            IMPORTING
              et_data     = <lt_data2>
              ev_where    = lv_where
          ).

          get_table_content(
            EXPORTING
              iv_table      = lv_table
              iv_where      = lv_where
              iv_rfcdest    = iv_rfcdest
              iv_client     = iv_client
              iv_from       = 1
              iv_upto       = gc_split_size
            IMPORTING
              et_data       = <lt_data>
              ev_error_text = ev_error_text
          ).
          IF ev_error_text IS NOT INITIAL.
            RETURN.
          ENDIF.


          CALL FUNCTION 'ZA2J_GET'
            DESTINATION iv_rfcdest
            EXPORTING
              iv_client             = iv_client
              iv_tname              = lv_table
              iv_flag_count_only    = abap_true
              iv_where              = lv_where
            IMPORTING
              ev_count              = lv_count
            EXCEPTIONS
              system_failure        = 101 MESSAGE ev_error_text
              communication_failure = 102 MESSAGE ev_error_text
              table_name_error      = 5
              sql_error             = 1
              unkown_error          = 2
              OTHERS                = 3.
          IF sy-subrc <> 0.
            IF ev_error_text IS INITIAL.
              ev_error_text = get_rfc_error_text( lv_table ).
            ENDIF.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.

          IF <lt_data> <> <lt_data2>.
            lv_diffrent = abap_true.
          ELSE.
            IF ls_file_and_table-parted EQ abap_false.
              " single file
              IF lv_count <> lines( <lt_data2> ).
                lv_diffrent = abap_true.
              ENDIF.
            ELSE.
              " next part
              lv_from = gc_split_size + 1.
              lv_part_num = 2.
              DO.
                CLEAR: <lt_data>, <lt_data2>.

                get_table_content(
                 EXPORTING
                   iv_table      = lv_table
                   iv_where      = lv_where
                   iv_rfcdest    = iv_rfcdest
                   iv_client     = iv_client
                   iv_from       = lv_from
                   iv_upto       = gc_split_size
                 IMPORTING
                   et_data       = <lt_data>
                   ev_error_text = ev_error_text
               ).
                IF ev_error_text IS NOT INITIAL.
                  RETURN.
                ENDIF.

                REPLACE REGEX '.part\d*.json.zip' IN lv_filename WITH '.part' && lv_part_num && '.json.zip'.
                READ TABLE lt_filename TRANSPORTING NO FIELDS WITH KEY table_line = lv_filename BINARY SEARCH.
                IF sy-subrc <> 0.
                  " file is not exist
                  IF <lt_data> IS NOT INITIAL.
                    lv_diffrent = abap_true.
                  ENDIF.
                  EXIT.
                ENDIF.

                file_upload(
                  EXPORTING
                    iv_filename  = lv_filename
                  IMPORTING
                    ev_xstring    = lv_xstring
                    ev_error_text = ev_error_text
                ).
                IF ev_error_text IS NOT INITIAL.
                  RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
                ENDIF.

                json2abap(
                  EXPORTING
                    iv_json_zip = lv_xstring
                  IMPORTING
                    et_data     = <lt_data2>
                ).

                IF <lt_data> <> <lt_data2>.
                  lv_diffrent = abap_true.
                  EXIT.
                ENDIF.

                lv_from = lv_from + gc_split_size.
                lv_part_num = lv_part_num + 1.
              ENDDO.

            ENDIF.
          ENDIF.

          " log
          IF ls_file_and_table-parted EQ abap_true.
            REPLACE REGEX '.part\d*.json.zip' IN lv_filename WITH '.part*.json.zip'.
          ENDIF.
          IF lv_diffrent EQ abap_true.
            APPEND VALUE #( table = lv_table file = lv_filename text = 'Different' count = lv_count ) TO et_log.
          ELSE.
            APPEND VALUE #( table = lv_table file = lv_filename text = 'Same' count = lv_count ) TO et_log.
          ENDIF.
        ENDLOOP.


      CATCH cx_demo_exception INTO lo_cx.
        ev_error_text = lo_cx->exception_text.
    ENDTRY.

  ENDMETHOD.


  METHOD file_upload.
    DATA: lv_filelength TYPE i,
          lt_temptable  TYPE w3mimetabtype,
          lo_cx         TYPE REF TO cx_demo_exception.

    CLEAR: ev_xstring, ev_error_text.

    IF iv_filename CP 'http*'.
      TRY.
          ev_xstring = get_http_response( iv_filename )->get_data( ).
        CATCH cx_demo_exception INTO lo_cx.
          ev_error_text = lo_cx->exception_text.
      ENDTRY.
    ELSE.
      cl_gui_frontend_services=>gui_upload(
        EXPORTING
          filename                = iv_filename              " Name of file
          filetype                = 'BIN'              " File Type (ASCII, Binary)
        IMPORTING
          filelength              = lv_filelength         " File Length
        CHANGING
          data_tab                = lt_temptable           " Transfer table for file contents
        EXCEPTIONS
          file_open_error         = 1                  " File does not exist and cannot be opened
          file_read_error         = 2                  " Error when reading file
          no_batch                = 3                  " Cannot execute front-end function in background
          gui_refuse_filetransfer = 4                  " Incorrect front end or error on front end
          invalid_type            = 5                  " Incorrect parameter FILETYPE
          no_authority            = 6                  " No upload authorization
          unknown_error           = 7                  " Unknown error
          bad_data_format         = 8                  " Cannot Interpret Data in File
          header_not_allowed      = 9                  " Invalid header
          separator_not_allowed   = 10                 " Invalid separator
          header_too_long         = 11                 " Header information currently restricted to 1023 bytes
          unknown_dp_error        = 12                 " Error when calling data provider
          access_denied           = 13                 " Access to file denied.
          dp_out_of_memory        = 14                 " Not enough memory in data provider
          disk_full               = 15                 " Storage medium is full.
          dp_timeout              = 16                 " Data provider timeout
          not_supported_by_gui    = 17                 " GUI does not support this
          error_no_gui            = 18                 " GUI not available
          OTHERS                  = 19
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_error_text.
        RETURN.
      ENDIF.

      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = lv_filelength
        IMPORTING
          buffer       = ev_xstring
        TABLES
          binary_tab   = lt_temptable
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_error_text.
        RETURN.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_table_content.
    DATA: lv_xstring    TYPE xstring,
          lv_client     TYPE mandt,
          lv_has_client TYPE flag,
          lv_offset     TYPE i.
    FIELD-SYMBOLS: <ls_data> TYPE data,
                   <lv_data> TYPE data.

    CLEAR: et_data, ev_error_text.

    CHECK: iv_table IS NOT INITIAL.
    IF iv_client IS NOT INITIAL.
      lv_client = iv_client.
    ELSE.
      lv_client = sy-mandt.
    ENDIF.

    IF iv_rfcdest IS NOT INITIAL.
      " RFC
      CALL FUNCTION 'ZA2J_GET'
        DESTINATION iv_rfcdest
        EXPORTING
          iv_client             = lv_client
          iv_table              = iv_table
          iv_from               = iv_from
          iv_upto               = iv_upto
          iv_where              = iv_where
        IMPORTING
          ev_json_zip           = lv_xstring
        EXCEPTIONS
          system_failure        = 101 MESSAGE ev_error_text
          communication_failure = 102 MESSAGE ev_error_text
          table_name_error      = 5
          sql_error             = 1
          unkown_error          = 2
          OTHERS                = 3.
      IF sy-subrc <> 0.
        IF ev_error_text IS INITIAL.
          ev_error_text = get_rfc_error_text( iv_table ).
        ENDIF.
        RETURN.
      ENDIF.

      json2abap(
        EXPORTING
          iv_json_zip = lv_xstring
        IMPORTING
          et_data     = et_data
      ).
    ELSE.
      " not RFC
      TRY.
          IF iv_upto IS NOT INITIAL.
            lv_offset = iv_from - 1.
            SELECT *
              FROM (iv_table)
              USING CLIENT @lv_client
              WHERE (iv_where)
              ORDER BY PRIMARY KEY
              INTO TABLE @et_data
              UP TO @iv_upto ROWS OFFSET @lv_offset.
          ELSE.
            SELECT *
              FROM (iv_table)
              USING CLIENT @lv_client
              WHERE (iv_where)
              INTO TABLE @et_data.
          ENDIF.

          " clear CLIENT(MANDT) field
          lv_has_client = cl_abap_typedescr=>describe_by_name( iv_table )->has_property( cl_abap_typedescr=>typepropkind_hasclient ).
          IF lv_has_client EQ abap_true.
            LOOP AT et_data ASSIGNING <ls_data>.
              ASSIGN COMPONENT 1 OF STRUCTURE <ls_data> TO <lv_data>.
              CLEAR: <lv_data>.
            ENDLOOP.
          ENDIF.

        CATCH cx_sy_sql_error.
          ev_error_text = TEXT-e01.
        CATCH cx_root.
          ev_error_text = TEXT-e02.
      ENDTRY.

    ENDIF.


  ENDMETHOD.


  METHOD file_download.
    DATA: lv_filelength     TYPE i,
          lt_temptable      TYPE w3mimetabtype,
          lv_file_separator TYPE c,
          lo_cx             TYPE REF TO cx_demo_exception.

    CLEAR: ev_error_text.

    IF iv_filename CP 'http*'.
      TRY.

          ev_error_text = 'export to github is not implemanted yet.'.
          RETURN.

        CATCH cx_demo_exception INTO lo_cx.
          ev_error_text = lo_cx->exception_text.
      ENDTRY.

    ELSE.

      cl_gui_frontend_services=>get_file_separator(
        CHANGING
          file_separator       = lv_file_separator
        EXCEPTIONS
          not_supported_by_gui = 1
          error_no_gui         = 2
          cntl_error           = 3
          OTHERS               = 4
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_error_text.
        RETURN.
      ENDIF.
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = iv_xstring
        IMPORTING
          output_length = lv_filelength
        TABLES
          binary_tab    = lt_temptable.
      cl_gui_frontend_services=>gui_download(
        EXPORTING
          bin_filesize              = lv_filelength         " File length for binary files
          filename                  = iv_folder && lv_file_separator && iv_filename             " Name of file
          filetype                  = 'BIN'                " File type (ASCII, binary ...)
          show_transfer_status      = abap_false
        CHANGING
          data_tab                  = lt_temptable             " Transfer table
        EXCEPTIONS
          file_write_error          = 1                    " Cannot write to file
          no_batch                  = 2                    " Cannot execute front-end function in background
          gui_refuse_filetransfer   = 3                    " Incorrect Front End
          invalid_type              = 4                    " Invalid value for parameter FILETYPE
          no_authority              = 5                    " No Download Authorization
          unknown_error             = 6                    " Unknown error
          header_not_allowed        = 7                    " Invalid header
          separator_not_allowed     = 8                    " Invalid separator
          filesize_not_allowed      = 9                    " Invalid file size
          header_too_long           = 10                   " Header information currently restricted to 1023 bytes
          dp_error_create           = 11                   " Cannot create DataProvider
          dp_error_send             = 12                   " Error Sending Data with DataProvider
          dp_error_write            = 13                   " Error Writing Data with DataProvider
          unknown_dp_error          = 14                   " Error when calling data provider
          access_denied             = 15                   " Access to file denied.
          dp_out_of_memory          = 16                   " Not enough memory in data provider
          disk_full                 = 17                   " Storage medium is full.
          dp_timeout                = 18                   " Data provider timeout
          file_not_found            = 19                   " Could not find file
          dataprovider_exception    = 20                   " General Exception Error in DataProvider
          control_flush_error       = 21                   " Error in Control Framework
          not_supported_by_gui      = 22                   " GUI does not support this
          error_no_gui              = 23                   " GUI not available
          OTHERS                    = 24
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_error_text.
        RETURN.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD get_http_response.
    DATA: lo_http    TYPE REF TO if_http_client,
          lv_message TYPE string.

    cl_http_client=>create_by_url(
      EXPORTING
        url                    = iv_url
      IMPORTING
        client                 = lo_http
      EXCEPTIONS
        OTHERS                 = 1
    ).
    CHECK: sy-subrc EQ 0.

    lo_http->send(
      EXCEPTIONS
        OTHERS                 = 1
    ).
    IF sy-subrc <> 0.
      lo_http->get_last_error(
        IMPORTING
          message        = lv_message
      ).
      RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = CONV #( lv_message ).
    ENDIF.
    CHECK: sy-subrc EQ 0.

    lo_http->receive(
      EXCEPTIONS
        OTHERS                 = 1
    ).
    IF sy-subrc <> 0.
      lo_http->get_last_error(
        IMPORTING
          message        = lv_message
      ).
      RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = CONV #( lv_message ).
    ENDIF.
    CHECK: sy-subrc EQ 0.

    ro_response = lo_http->response.
    IF ro_response IS INITIAL.
      RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = CONV #( iv_url ).
    ENDIF.

*    rv_response = lo_http->response->get_cdata( ).

  ENDMETHOD.


  METHOD get_json_zip_file_list.
    CONSTANTS: lc_github_url  TYPE string VALUE 'https://github.com/',
               lc_rawfile_url TYPE string VALUE 'https://raw.githubusercontent.com/'.
    DATA: lv_base_url       TYPE string,
          lv_html           TYPE string,
          lv_regex          TYPE string,
          lt_match_result   TYPE match_result_tab,
          ls_match_result   TYPE match_result,
          lv_filename       TYPE string,
          lv_file_separator TYPE c,
          lt_file_info      TYPE TABLE OF file_info,
          ls_file_info      TYPE file_info,
          lv_count          TYPE i,
          lv_error_text     TYPE text255.



    IF iv_folder CP 'http*'.

      lv_base_url = iv_folder.
      IF lv_base_url CP 'https://github.com/*'.
        " github.com
        IF lv_base_url NS '/tree/'.
          lv_base_url = lv_base_url && '/tree/main/'.
        ENDIF.

        lv_html = get_http_response( lv_base_url )->get_cdata( ).
        CHECK: lv_html IS NOT INITIAL.
        REPLACE lc_github_url IN lv_base_url WITH ''.

        " file
        lv_regex = lv_base_url && '[^"]*\.json\.zip'.
        REPLACE '/tree/' IN lv_regex WITH '/blob/'.
        FIND ALL OCCURRENCES OF REGEX lv_regex IN lv_html RESULTS lt_match_result.
        LOOP AT lt_match_result INTO ls_match_result.
          lv_filename = lc_rawfile_url && lv_html+ls_match_result-offset(ls_match_result-length).
          REPLACE '/blob/' IN lv_filename WITH '/'.
          APPEND lv_filename TO rt_filename.
        ENDLOOP.

        " folder
        IF iv_sub_folder EQ abap_true.
          lv_regex = lv_base_url && '[^"]*'.
          FIND ALL OCCURRENCES OF REGEX lv_regex IN lv_html RESULTS lt_match_result.
          LOOP AT lt_match_result INTO ls_match_result.
            lv_filename = lc_github_url && lv_html+ls_match_result-offset(ls_match_result-length).
            APPEND LINES OF get_json_zip_file_list( lv_filename ) TO rt_filename.
          ENDLOOP.
        ENDIF.

      ELSE.
        " others
        IF lv_base_url CS '.json'.
          APPEND lv_base_url TO rt_filename.
        ENDIF.
      ENDIF.

    ELSE.

      CHECK: iv_folder IS NOT INITIAL.

      cl_gui_frontend_services=>get_file_separator(
        CHANGING
          file_separator       = lv_file_separator
        EXCEPTIONS
          not_supported_by_gui = 1
          error_no_gui         = 2
          cntl_error           = 3
          OTHERS               = 4
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_error_text.
        RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = lv_error_text.
      ENDIF.

      " file
      CLEAR: lt_file_info, lv_count.
      cl_gui_frontend_services=>directory_list_files(
        EXPORTING
          directory                   = iv_folder        " Directory To Search
          filter                      = '*.json.zip'            " File filter
          files_only                  = abap_true       " Return only Files, no Directories
        CHANGING
          file_table                  = lt_file_info       " Return Table for the Found Files
          count                       = lv_count            " Number of Files/Dir Found
        EXCEPTIONS
          cntl_error                  = 1                " Control error
          directory_list_files_failed = 2                " Could not list files in the directory
          wrong_parameter             = 3                " Incorrect parameter combination
          error_no_gui                = 4                " No GUI available
          not_supported_by_gui        = 5                " GUI does not support this
          OTHERS                      = 6
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_error_text.
        RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = lv_error_text.
      ENDIF.

      LOOP AT lt_file_info INTO ls_file_info.
        lv_filename = iv_folder && lv_file_separator && ls_file_info-filename.
        APPEND lv_filename TO rt_filename.
      ENDLOOP.

      " folder
      IF iv_sub_folder EQ abap_true.
        CLEAR: lt_file_info, lv_count.
        cl_gui_frontend_services=>directory_list_files(
          EXPORTING
            directory                   = iv_folder        " Directory To Search
            directories_only            = abap_true " Return only Directories, no Files
          CHANGING
            file_table                  = lt_file_info       " Return Table for the Found Files
            count                       = lv_count            " Number of Files/Dir Found
          EXCEPTIONS
            cntl_error                  = 1                " Control error
            directory_list_files_failed = 2                " Could not list files in the directory
            wrong_parameter             = 3                " Incorrect parameter combination
            error_no_gui                = 4                " No GUI available
            not_supported_by_gui        = 5                " GUI does not support this
            OTHERS                      = 6
        ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_error_text.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = lv_error_text.
        ENDIF.

        LOOP AT lt_file_info INTO ls_file_info.
          lv_filename = iv_folder && lv_file_separator && ls_file_info-filename.
          APPEND LINES OF get_json_zip_file_list( lv_filename ) TO rt_filename.
        ENDLOOP.
      ENDIF.

      IF rt_filename IS INITIAL AND iv_folder CS '.json'.
        " if iv_folder is file (not folder), return it.
        IF cl_gui_frontend_services=>file_exist( iv_folder ) EQ abap_true.
          APPEND iv_folder TO rt_filename.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_table_name.
    DATA: lv_name   TYPE string,
          lv_offset TYPE i.

    " reemove before /
    FIND ALL OCCURRENCES OF REGEX '[\\/]' IN iv_filename MATCH OFFSET lv_offset.
    IF sy-subrc EQ 0.
      lv_offset = lv_offset + 1.
    ENDIF.
    lv_name = iv_filename+lv_offset.

    " remove after .
    FIND '.' IN lv_name MATCH OFFSET lv_offset.
    rv_table = lv_name(lv_offset).
  ENDMETHOD.
ENDCLASS.
