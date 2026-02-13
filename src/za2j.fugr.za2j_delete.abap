FUNCTION za2j_delete.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_CLIENT) TYPE  MANDT
*"     VALUE(IV_TNAME) TYPE  TABNAME
*"     VALUE(IV_WHERE) TYPE  STRING OPTIONAL
*"     VALUE(IV_COMMIT_WORK) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     VALUE(EV_COUNT) TYPE  I
*"  EXCEPTIONS
*"      SQL_ERROR
*"      UNKOWN_ERROR
*"----------------------------------------------------------------------
  DATA: lv_client    TYPE mandt,
        lo_exception TYPE REF TO cx_root.

  CLEAR: ev_count.

  CHECK: iv_tname IS NOT INITIAL.
  IF iv_client IS NOT INITIAL.
    lv_client = iv_client.
  ELSE.
    lv_client = sy-mandt.
  ENDIF.


  TRY .
      DELETE FROM (iv_tname) USING CLIENT @lv_client WHERE (iv_where).
      ev_count = sy-dbcnt.
      IF iv_commit_work EQ abap_true.
        COMMIT WORK.
      ENDIF.

    CATCH cx_sy_dynamic_osql_error INTO lo_exception.
      RAISE sql_error.
    CATCH cx_root INTO lo_exception.
      RAISE unkown_error.
  ENDTRY.

  IF zcl_abap2json=>gc_trace_log_on EQ abap_true.
    DATA: ls_zta2j_trace_log TYPE zta2j_trace_log.
    ls_zta2j_trace_log-datum = sy-datum.
    ls_zta2j_trace_log-uzeit = sy-uzeit.
    ls_zta2j_trace_log-uname = sy-uname.
    ls_zta2j_trace_log-tabname = iv_tname.
    ls_zta2j_trace_log-op = 'ZA2J_DELETE'.
    ls_zta2j_trace_log-dbcnt = ev_count.
    ls_zta2j_trace_log-sql_where = iv_where.
    MODIFY zta2j_trace_log USING CLIENT @lv_client FROM @ls_zta2j_trace_log.
  ENDIF.

ENDFUNCTION.
