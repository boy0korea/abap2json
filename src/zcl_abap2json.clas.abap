CLASS zcl_abap2json DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_log,
        table TYPE tabname,
        file  TYPE string,
        text  TYPE string,
        count TYPE i,
      END OF ts_log .
    TYPES:
      tt_log TYPE TABLE OF ts_log .

    CONSTANTS gc_split_size TYPE i VALUE 10000 ##NO_TEXT.
    CONSTANTS gc_encoding TYPE abap_encod VALUE 'UTF-8' ##NO_TEXT.
    CLASS-DATA gt_tms TYPE stms_wbo_clients READ-ONLY .
    CLASS-DATA gt_server_info TYPE vrm_values READ-ONLY .

    CLASS-METHODS abap2json
      IMPORTING
        !it_data     TYPE data
        !iv_where    TYPE string OPTIONAL
      EXPORTING
        !ev_json     TYPE string
        !ev_json_zip TYPE xstring .
    CLASS-METHODS json2abap
      IMPORTING
        !iv_json_zip TYPE xstring
      EXPORTING
        !et_data     TYPE data
        !ev_where    TYPE string .
    CLASS-METHODS import_json_zip
      IMPORTING
        !iv_folder            TYPE clike OPTIONAL
        !iv_del               TYPE flag OPTIONAL
        !iv_simulate          TYPE flag OPTIONAL
        !iv_show_progress_bar TYPE flag OPTIONAL
        !iv_rfcdest           TYPE rfcdest OPTIONAL
        !iv_client            TYPE mandt OPTIONAL
      EXPORTING
        !et_log               TYPE tt_log
        !ev_error_text        TYPE char255 .
    CLASS-METHODS compare_json_zip
      IMPORTING
        !iv_folder            TYPE clike OPTIONAL
        !iv_show_progress_bar TYPE flag OPTIONAL
        !iv_rfcdest           TYPE rfcdest OPTIONAL
        !iv_client            TYPE mandt OPTIONAL
      EXPORTING
        !et_log               TYPE tt_log
        !ev_error_text        TYPE char255 .
    CLASS-METHODS export_table
      IMPORTING
        !iv_table             TYPE tabname
        !iv_where             TYPE string OPTIONAL
        !iv_folder            TYPE clike OPTIONAL
        !iv_show_progress_bar TYPE flag OPTIONAL
        !iv_rfcdest           TYPE rfcdest OPTIONAL
        !iv_client            TYPE mandt OPTIONAL
      EXPORTING
        !et_log               TYPE tt_log
        !ev_error_text        TYPE char255 .
    CLASS-METHODS export_package
      IMPORTING
        !iv_package           TYPE devclass
        !iv_folder            TYPE clike OPTIONAL
        !iv_show_progress_bar TYPE flag OPTIONAL
        !iv_rfcdest           TYPE rfcdest OPTIONAL
        !iv_client            TYPE mandt OPTIONAL
      EXPORTING
        !et_log               TYPE tt_log
        !ev_error_text        TYPE char255 .
    CLASS-METHODS compare_table
      IMPORTING
        !iv_table             TYPE tabname
        !iv_where             TYPE string OPTIONAL
        !iv_show_progress_bar TYPE flag OPTIONAL
        !iv_rfcdest           TYPE rfcdest OPTIONAL
        !iv_client            TYPE mandt OPTIONAL
        !iv_rfcdest2          TYPE rfcdest OPTIONAL
        !iv_client2           TYPE mandt OPTIONAL
      EXPORTING
        !ev_same              TYPE flag
        !ev_error_text        TYPE char255 .
    CLASS-METHODS copy_table
      IMPORTING
        !iv_table             TYPE tabname
        !iv_where             TYPE string OPTIONAL
        !iv_del               TYPE flag OPTIONAL
        !iv_simulate          TYPE flag OPTIONAL
        !iv_show_progress_bar TYPE flag OPTIONAL
        !iv_rfcdest           TYPE rfcdest OPTIONAL
        !iv_client            TYPE mandt OPTIONAL
        !iv_rfcdest2          TYPE rfcdest OPTIONAL
        !iv_client2           TYPE mandt OPTIONAL
      EXPORTING
        !et_log               TYPE tt_log
        !ev_error_text        TYPE char255 .
    CLASS-METHODS init_server_info
      RETURNING
        VALUE(rt_dd) TYPE vrm_values .
    CLASS-METHODS get_server_info
      IMPORTING
        !iv_server  TYPE clike
      EXPORTING
        !ev_rfcdest TYPE rfcdest
        !ev_client  TYPE mandt .
    CLASS-METHODS get_rfc_error_text
      RETURNING
        VALUE(rv_error_text) TYPE text255 .
  PROTECTED SECTION.
  PRIVATE SECTION.

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
    CLASS-METHODS file_upload
      IMPORTING
        !iv_file_name  TYPE string
      EXPORTING
        !ev_xstring    TYPE xstring
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

    TRY.

        IF iv_rfcdest IS NOT INITIAL AND iv_client IS INITIAL.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = TEXT-e03.
        ENDIF.
        IF iv_rfcdest2 IS NOT INITIAL AND iv_client2 IS INITIAL.
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
            sql_error             = 1
            unkown_error          = 2
            OTHERS                = 3.
        IF sy-subrc <> 0.
          IF ev_error_text IS INITIAL.
            ev_error_text = get_rfc_error_text( ).
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
            sql_error             = 1
            unkown_error          = 2
            OTHERS                = 3.
        IF sy-subrc <> 0.
          IF ev_error_text IS INITIAL.
            ev_error_text = get_rfc_error_text( ).
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

    TRY.

        IF iv_rfcdest IS NOT INITIAL AND iv_client IS INITIAL.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = TEXT-e03.
        ENDIF.
        IF iv_rfcdest2 IS NOT INITIAL AND iv_client2 IS INITIAL.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = TEXT-e03.
        ENDIF.

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
              sql_error             = 1
              unkown_error          = 2
              OTHERS                = 3.
          IF sy-subrc <> 0.
            IF ev_error_text IS INITIAL.
              ev_error_text = get_rfc_error_text( ).
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
            sql_error             = 1
            unkown_error          = 2
            OTHERS                = 3.
        IF sy-subrc <> 0.
          IF ev_error_text IS INITIAL.
            ev_error_text = get_rfc_error_text( ).
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
              sql_error             = 1
              unkown_error          = 2
              OTHERS                = 3.
          IF sy-subrc <> 0.
            IF ev_error_text IS INITIAL.
              ev_error_text = get_rfc_error_text( ).
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
              sql_error             = 1
              unkown_error          = 2
              OTHERS                = 3.
          IF sy-subrc <> 0.
            IF ev_error_text IS INITIAL.
              ev_error_text = get_rfc_error_text( ).
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
    DATA: lv_selected_folder TYPE string,
          lt_table           TYPE TABLE OF tabname,
          lv_table           TYPE tabname,
          lt_log             TYPE tt_log,
          lv_total           TYPE i,
          lv_index           TYPE i,
          lo_cx              TYPE REF TO cx_demo_exception.

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


        IF iv_folder IS NOT INITIAL.
          lv_selected_folder = iv_folder.
        ELSE.
          cl_gui_frontend_services=>directory_browse(
            CHANGING
              selected_folder      = lv_selected_folder
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
        CHECK: lv_selected_folder IS NOT INITIAL.


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
              iv_folder            = lv_selected_folder
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
    DATA: lv_file_separator  TYPE c,
          lv_selected_folder TYPE string,
          lv_datetime        TYPE string,
          lv_file_name       TYPE string,
          lv_filelength      TYPE i,
          lt_temptable       TYPE w3mimetabtype,
          lv_xstring         TYPE xstring,
          lv_total           TYPE i,
          lv_from            TYPE i,
          lv_part_num        TYPE i,
          lv_count           TYPE i,
          lv_index           TYPE i,
          lo_cx              TYPE REF TO cx_demo_exception.

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
            sql_error             = 1
            unkown_error          = 2
            OTHERS                = 3.
        IF sy-subrc <> 0.
          IF ev_error_text IS INITIAL.
            ev_error_text = get_rfc_error_text( ).
          ENDIF.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
        ENDIF.

        IF lv_total EQ 0.
          APPEND VALUE #( table = iv_table text = 'empty' count = 0 ) TO et_log.
          RETURN.
        ENDIF.


        IF iv_folder IS NOT INITIAL.
          lv_selected_folder = iv_folder.
        ELSE.
          cl_gui_frontend_services=>directory_browse(
            CHANGING
              selected_folder      = lv_selected_folder
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

        CHECK: lv_selected_folder IS NOT INITIAL.

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
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
        ENDIF.


        lv_datetime = sy-datum && sy-uzeit.
        lv_from = 1.
        WHILE lv_from <= lv_total.
          CLEAR: lv_xstring, lv_count, lt_temptable.

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
              sql_error             = 1
              unkown_error          = 2
              OTHERS                = 3.
          IF sy-subrc <> 0.
            IF ev_error_text IS INITIAL.
              ev_error_text = get_rfc_error_text( ).
            ENDIF.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.


          IF lv_count IS INITIAL.
            EXIT.
          ELSE.
            lv_from = lv_from + gc_split_size.
            lv_part_num = lv_part_num + 1.
            IF lv_part_num EQ 1 AND lv_total <= gc_split_size.
              lv_file_name = iv_table && '.' && lv_datetime && '.json.zip'.
            ELSE.
              lv_file_name = iv_table && '.' && lv_datetime && '.part' && lv_part_num && '.json.zip'.
            ENDIF.

            CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
              EXPORTING
                buffer        = lv_xstring
              IMPORTING
                output_length = lv_filelength
              TABLES
                binary_tab    = lt_temptable.
            cl_gui_frontend_services=>gui_download(
              EXPORTING
                bin_filesize              = lv_filelength         " File length for binary files
                filename                  = lv_selected_folder && lv_file_separator && lv_file_name             " Name of file
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
              RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
            ENDIF.

            APPEND VALUE #( table = iv_table file = lv_file_name count = lv_count ) TO et_log.

          ENDIF.
        ENDWHILE.


      CATCH cx_demo_exception INTO lo_cx.
        ev_error_text = lo_cx->exception_text.
    ENDTRY.

  ENDMETHOD.


  METHOD get_rfc_error_text.
    CASE sy-subrc.
      WHEN 1.
        rv_error_text = TEXT-e01.
      WHEN OTHERS.
        rv_error_text = TEXT-e02.
    ENDCASE.
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
    DATA: lv_file_separator  TYPE c,
          lv_selected_folder TYPE string,
          lt_file_info       TYPE TABLE OF file_info,
          ls_file_info       TYPE file_info,
          lv_file_name       TYPE string,
          lv_xstring         TYPE xstring,
          lv_json            TYPE string,
          lt_string          TYPE TABLE OF string,
          lv_string          TYPE string,
          lt_table           TYPE TABLE OF tabname,
          lv_table           TYPE tabname,
          lv_count           TYPE i,
          lv_total           TYPE i,
          lv_index           TYPE i,
          lo_cx              TYPE REF TO cx_demo_exception.

    CLEAR: et_log, ev_error_text.

    TRY.

        IF iv_rfcdest IS NOT INITIAL AND iv_client IS INITIAL.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = TEXT-e03.
        ENDIF.


        IF iv_folder IS NOT INITIAL.
          lv_selected_folder = iv_folder.
        ELSE.
          cl_gui_frontend_services=>directory_browse(
            CHANGING
              selected_folder      = lv_selected_folder
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

        CHECK: lv_selected_folder IS NOT INITIAL.

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
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
        ENDIF.


        cl_gui_frontend_services=>directory_list_files(
          EXPORTING
            directory                   = lv_selected_folder        " Directory To Search
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
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_error_text.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
        ENDIF.

        CHECK: lt_file_info IS NOT INITIAL.


        IF iv_del EQ abap_true.
          LOOP AT lt_file_info INTO ls_file_info.
            SPLIT ls_file_info-filename AT '.' INTO TABLE lt_string.
            READ TABLE lt_string INTO lv_table INDEX 1.
            APPEND lv_table TO lt_table.
          ENDLOOP.
          SORT lt_table.
          DELETE ADJACENT DUPLICATES FROM lt_table.
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
                sql_error             = 1
                unkown_error          = 2
                OTHERS                = 3.
            IF sy-subrc <> 0.
              IF ev_error_text IS INITIAL.
                ev_error_text = get_rfc_error_text( ).
              ENDIF.
              RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
            ENDIF.
            APPEND VALUE #( table = lv_table text = 'deleted' count = lv_count ) TO et_log.
          ENDLOOP.
        ENDIF.

        lv_total = lines( lt_file_info ).
        LOOP AT lt_file_info INTO ls_file_info.
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

          lv_file_name = lv_selected_folder && lv_file_separator && ls_file_info-filename.
          file_upload(
            EXPORTING
              iv_file_name  = lv_file_name
            IMPORTING
              ev_xstring    = lv_xstring
              ev_error_text = ev_error_text
          ).
          IF ev_error_text IS NOT INITIAL.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.

          SPLIT ls_file_info-filename AT '.' INTO TABLE lt_string.
          READ TABLE lt_string INTO lv_table INDEX 1.

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
              sql_error             = 1
              unkown_error          = 2
              OTHERS                = 3.
          IF sy-subrc <> 0.
            IF ev_error_text IS INITIAL.
              ev_error_text = get_rfc_error_text( ).
            ENDIF.
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.

          APPEND VALUE #( table = lv_table file = ls_file_info-filename count = lv_count ) TO et_log.
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
    IF sy-subrc <> 0.
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
    DATA: lv_file_separator  TYPE c,
          lv_selected_folder TYPE string,
          lt_file_info       TYPE TABLE OF file_info,
          ls_file_info       TYPE file_info,
          BEGIN OF ls_file_and_table,
            filename TYPE string,
            table    TYPE string,
            parted   TYPE flag,
          END OF ls_file_and_table,
          lt_file_and_table LIKE TABLE OF ls_file_and_table,
          lv_file_name      TYPE string,
          lv_xstring        TYPE xstring,
          lv_json           TYPE string,
          lt_string         TYPE TABLE OF string,
          lv_string         TYPE string,
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
                   <lt_data2> TYPE table,
                   <ls_data>  TYPE data,
                   <lv_data>  TYPE data.

    CLEAR: et_log, ev_error_text.

    TRY.

        IF iv_rfcdest IS NOT INITIAL AND iv_client IS INITIAL.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = TEXT-e03.
        ENDIF.


        IF iv_folder IS NOT INITIAL.
          lv_selected_folder = iv_folder.
        ELSE.
          cl_gui_frontend_services=>directory_browse(
            CHANGING
              selected_folder      = lv_selected_folder
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

        CHECK: lv_selected_folder IS NOT INITIAL.

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
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
        ENDIF.


        cl_gui_frontend_services=>directory_list_files(
          EXPORTING
            directory                   = lv_selected_folder        " Directory To Search
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
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_error_text.
          RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
        ENDIF.

        CHECK: lt_file_info IS NOT INITIAL.
        SORT lt_file_info BY filename.

        LOOP AT lt_file_info INTO ls_file_info.
          CLEAR: ls_file_and_table.
          ls_file_and_table-filename = ls_file_info-filename.
          REPLACE REGEX '.part\d*.json.zip' IN ls_file_and_table-filename WITH '.part1.json.zip'.
          IF sy-subrc EQ 0.
            IF ls_file_and_table-filename EQ lv_file_name.
              CONTINUE.
            ENDIF.
            lv_file_name = ls_file_and_table-filename.
            ls_file_and_table-parted = abap_true.
          ENDIF.
          FIND '.' IN ls_file_and_table-filename MATCH OFFSET lv_count.
          ls_file_and_table-table = ls_file_and_table-filename(lv_count).
          APPEND ls_file_and_table TO lt_file_and_table.
        ENDLOOP.



        lv_total = lines( lt_file_and_table ).
        LOOP AT lt_file_and_table INTO ls_file_and_table.
          CLEAR: lv_diffrent.

          lv_index = sy-tabix.
          lv_table = ls_file_and_table-table.

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


          lv_file_name = lv_selected_folder && lv_file_separator && ls_file_and_table-filename.
          file_upload(
            EXPORTING
              iv_file_name  = lv_file_name
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
              sql_error             = 1
              unkown_error          = 2
              OTHERS                = 3.
          IF sy-subrc <> 0.
            IF ev_error_text IS INITIAL.
              ev_error_text = get_rfc_error_text( ).
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

                REPLACE REGEX '.part\d*.json.zip' IN ls_file_and_table-filename WITH '.part' && lv_part_num && '.json.zip'.
                READ TABLE lt_file_info TRANSPORTING NO FIELDS WITH KEY filename = ls_file_and_table-filename BINARY SEARCH.
                IF sy-subrc <> 0.
                  " file is not exist
                  IF <lt_data> IS NOT INITIAL.
                    lv_diffrent = abap_true.
                  ENDIF.
                  EXIT.
                ENDIF.

                lv_file_name = lv_selected_folder && lv_file_separator && ls_file_and_table-filename.
                file_upload(
                  EXPORTING
                    iv_file_name  = lv_file_name
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
            REPLACE REGEX '.part\d*.json.zip' IN ls_file_and_table-filename WITH '.part*.json.zip'.
          ENDIF.
          IF lv_diffrent EQ abap_true.
            APPEND VALUE #( table = lv_table file = ls_file_and_table-filename text = 'Different' count = lv_count ) TO et_log.
          ELSE.
            APPEND VALUE #( table = lv_table file = ls_file_and_table-filename text = 'Same' count = lv_count ) TO et_log.
          ENDIF.
        ENDLOOP.


      CATCH cx_demo_exception INTO lo_cx.
        ev_error_text = lo_cx->exception_text.
    ENDTRY.

  ENDMETHOD.


  METHOD file_upload.
    DATA: lv_filelength TYPE i,
          lt_temptable  TYPE w3mimetabtype.

    CLEAR: ev_xstring, ev_error_text.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = iv_file_name              " Name of file
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
          sql_error             = 1
          unkown_error          = 2
          OTHERS                = 3.
      IF sy-subrc <> 0.
        IF ev_error_text IS INITIAL.
          ev_error_text = get_rfc_error_text( ).
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

        CATCH cx_sy_dynamic_osql_error.
          ev_error_text = TEXT-e01.
        CATCH cx_root.
          ev_error_text = TEXT-e02.
      ENDTRY.

    ENDIF.


  ENDMETHOD.
ENDCLASS.
