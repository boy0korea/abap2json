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
*"      TABLE_NAME_ERROR
*"      SQL_ERROR
*"      UNKOWN_ERROR
*"----------------------------------------------------------------------
  DATA: lv_client TYPE mandt,
        ltr_data  TYPE REF TO data.

  CLEAR: ev_count.

  CHECK: iv_tname IS NOT INITIAL.
  IF iv_client IS NOT INITIAL.
    lv_client = iv_client.
  ELSE.
    lv_client = sy-mandt.
  ENDIF.


  TRY .
      CREATE DATA ltr_data TYPE TABLE OF (iv_tname).  " for TABLE_NAME_ERROR

      DELETE FROM (iv_tname) USING CLIENT @lv_client WHERE (iv_where).
      ev_count = sy-dbcnt.
      IF iv_commit_work EQ abap_true.
        COMMIT WORK.
      ENDIF.

    CATCH cx_sy_create_error.
      RAISE table_name_error.
    CATCH cx_sy_sql_error.
      RAISE sql_error.
    CATCH cx_root.
      RAISE unkown_error.
  ENDTRY.

ENDFUNCTION.
