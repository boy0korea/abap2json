CLASS zcl_abap2json DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_log,
        table TYPE string,
        file  TYPE string,
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
      EXPORTING
        !ev_json     TYPE string
        !ev_json_zip TYPE xstring .
    CLASS-METHODS json2abap
      IMPORTING
        !iv_json_zip TYPE xstring
      EXPORTING
        !et_data     TYPE data .
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
ENDCLASS.



CLASS ZCL_ABAP2JSON IMPLEMENTATION.


  METHOD abap2json.
    DATA: lv_xstring TYPE xstring,
          lo_zip     TYPE REF TO cl_abap_zip.


    ev_json = /ui2/cl_json=>serialize( it_data ).


    CHECK: ev_json_zip IS REQUESTED.

    cl_abap_conv_out_ce=>create( encoding = gc_encoding )->convert(
      EXPORTING
        data   = ev_json
      IMPORTING
        buffer = lv_xstring
    ).

    CREATE OBJECT lo_zip.
    lo_zip->add(
      EXPORTING
        name           = 'a.json'
        content        = lv_xstring
    ).

    ev_json_zip = lo_zip->save( ).

  ENDMETHOD.


  METHOD compare_table.
    DATA: lv_xstring    TYPE xstring,
          lv_xstring2   TYPE xstring,
          ltr_data      TYPE REF TO data,
          lv_total      TYPE i,
          lv_total2     TYPE i,
          lv_from       TYPE i,
          lv_count      TYPE i,
          lv_count2     TYPE i,
          lv_index      TYPE i,
          lv_has_client TYPE flag,
          lo_cx         TYPE REF TO cx_demo_exception.
    FIELD-SYMBOLS: <lt_data>  TYPE table,
                   <lt_data2> TYPE table,
                   <ls_data>  TYPE data,
                   <lv_data>  TYPE data.

    CLEAR: ev_same.

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


        lv_has_client = cl_abap_typedescr=>describe_by_name( iv_table )->has_property( cl_abap_typedescr=>typepropkind_hasclient ).

        CREATE DATA ltr_data TYPE TABLE OF (iv_table).
        ASSIGN ltr_data->* TO <lt_data>.
        CREATE DATA ltr_data TYPE TABLE OF (iv_table).
        ASSIGN ltr_data->* TO <lt_data2>.

        lv_from = 1.
        WHILE lv_from <= lv_total.
          CLEAR: lv_xstring, lv_count.

          IF iv_show_progress_bar EQ abap_true.
            CALL FUNCTION 'PROGRESS_INDICATOR'
              EXPORTING
                i_text      = |{ CONV f( 100 * lv_from / lv_total ) DECIMALS = 2 }% of { lv_total NUMBER = USER }|
                i_processed = lv_from
                i_total     = lv_total.
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

          CALL FUNCTION 'ZA2J_GET'
            DESTINATION iv_rfcdest2
            EXPORTING
              iv_client             = iv_client2
              iv_tname              = iv_table
              iv_from               = lv_from
              iv_upto               = gc_split_size
              iv_where              = iv_where
            IMPORTING
              ev_json_zip           = lv_xstring2
              ev_count              = lv_count2
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


          IF lv_count IS INITIAL AND lv_count2 IS INITIAL.
            EXIT.
          ELSE.
            lv_from = lv_from + gc_split_size.
            IF lv_count <> lv_count2.
              RETURN.
            ENDIF.
            CLEAR: <lt_data>, <lt_data2>.
            json2abap(
              EXPORTING
                iv_json_zip = lv_xstring
              IMPORTING
                et_data     = <lt_data>
            ).
            json2abap(
              EXPORTING
                iv_json_zip = lv_xstring2
              IMPORTING
                et_data     = <lt_data2>
            ).
            IF lv_has_client EQ abap_true.
              LOOP AT <lt_data> ASSIGNING <ls_data>.
                ASSIGN COMPONENT 1 OF STRUCTURE <ls_data> TO <lv_data>.
                CLEAR: <lv_data>.
              ENDLOOP.
              LOOP AT <lt_data2> ASSIGNING <ls_data>.
                ASSIGN COMPONENT 1 OF STRUCTURE <ls_data> TO <lv_data>.
                CLEAR: <lv_data>.
              ENDLOOP.
            ENDIF.
            IF <lt_data> <> <lt_data2>.
              RETURN.
            ENDIF.
          ENDIF.
        ENDWHILE.


        ev_same = abap_true.

      CATCH cx_demo_exception INTO lo_cx.
        ev_error_text = lo_cx->exception_text.
    ENDTRY.

  ENDMETHOD.


  METHOD copy_table.
    DATA: lv_xstring    TYPE xstring,
          lv_xstring2   TYPE xstring,
          ltr_data      TYPE REF TO data,
          lv_total      TYPE i,
          lv_total2     TYPE i,
          lv_from       TYPE i,
          lv_count      TYPE i,
          lv_count2     TYPE i,
          lv_index      TYPE i,
          lv_has_client TYPE flag,
          lo_cx         TYPE REF TO cx_demo_exception.
    CLEAR: et_log.

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
          APPEND VALUE #( table = iv_table file = 'deleted' count = lv_count ) TO et_log.
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
                i_text      = |{ CONV f( 100 * lv_from / lv_total ) DECIMALS = 2 }% of { lv_total NUMBER = USER }|
                i_processed = lv_from
                i_total     = lv_total.
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

          APPEND VALUE #( table = iv_table file = 'copy' count = lv_count ) TO et_log.
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

    CLEAR: et_log.

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
                i_text      = |{ CONV f( 100 * lv_index / lv_total ) DECIMALS = 2 }% { lv_table }|
                i_processed = lv_index
                i_total     = lv_total.
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

    CLEAR: et_log.

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
          APPEND VALUE #( table = iv_table count = 0 ) TO et_log.
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



        lv_from = 1.
        WHILE lv_from <= lv_total.
          CLEAR: lv_xstring, lv_count, lt_temptable.

          IF iv_show_progress_bar EQ abap_true.
            CALL FUNCTION 'PROGRESS_INDICATOR'
              EXPORTING
                i_text      = |{ CONV f( 100 * lv_from / lv_total ) DECIMALS = 2 }% of { lv_total NUMBER = USER }|
                i_processed = lv_from
                i_total     = lv_total.
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

            CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
              EXPORTING
                buffer        = lv_xstring
              IMPORTING
                output_length = lv_filelength
              TABLES
                binary_tab    = lt_temptable.

            IF lv_part_num EQ 1 AND lv_total <= gc_split_size.
              lv_file_name = iv_table && '.' && sy-datum && '.json.zip'.
            ELSE.
              lv_file_name = iv_table && '.' && sy-datum && '.part' && lv_part_num && '.json.zip'.
            ENDIF.
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
          lv_filelength      TYPE i,
          lt_temptable       TYPE w3mimetabtype,
          lv_xstring         TYPE xstring,
          lv_json            TYPE string,
          lt_string          TYPE TABLE OF string,
          lv_string          TYPE string,
          lt_table           TYPE TABLE OF tabname,
          lv_table           TYPE tabname,
*          lr_data            TYPE REF TO data,
          lv_count           TYPE i,
          lv_total           TYPE i,
          lv_index           TYPE i,
          lo_cx              TYPE REF TO cx_demo_exception.
*    FIELD-SYMBOLS: <lt_data> TYPE table.

    CLEAR: et_log.

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
            APPEND VALUE #( table = lv_table file = 'deleted' count = lv_count ) TO et_log.
          ENDLOOP.
        ENDIF.

        lv_total = lines( lt_file_info ).
        LOOP AT lt_file_info INTO ls_file_info.
          CLEAR: lv_table, lv_filelength, lt_temptable.

          lv_index = sy-tabix.

          IF iv_show_progress_bar EQ abap_true.
            CALL FUNCTION 'PROGRESS_INDICATOR'
              EXPORTING
                i_text      = |{ CONV f( 100 * lv_index / lv_total ) DECIMALS = 2 }% { lv_table }|
                i_processed = lv_index
                i_total     = lv_total.
          ENDIF.

          lv_file_name = lv_selected_folder && lv_file_separator && ls_file_info-filename.
          cl_gui_frontend_services=>gui_upload(
            EXPORTING
              filename                = lv_file_name              " Name of file
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
            RAISE EXCEPTION TYPE cx_demo_exception EXPORTING exception_text = ev_error_text.
          ENDIF.

          CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
            EXPORTING
              input_length = lv_filelength
            IMPORTING
              buffer       = lv_xstring
            TABLES
              binary_tab   = lt_temptable
            EXCEPTIONS
              failed       = 1
              OTHERS       = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_error_text.
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

    CREATE OBJECT lo_zip.
    lo_zip->load(
      EXPORTING
        zip             = iv_json_zip
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2
    ).
    CHECK: sy-subrc EQ 0.

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
ENDCLASS.
