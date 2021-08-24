FUNCTION za2j_get.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_CLIENT) TYPE  MANDT
*"     VALUE(IV_TNAME) TYPE  TABNAME
*"     VALUE(IV_FROM) TYPE  I OPTIONAL
*"     VALUE(IV_UPTO) TYPE  I OPTIONAL
*"     VALUE(IV_FLAG_COUNT_ONLY) TYPE  FLAG OPTIONAL
*"     VALUE(IV_WHERE) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     VALUE(EV_JSON_ZIP) TYPE  XSTRING
*"     VALUE(EV_COUNT) TYPE  I
*"  EXCEPTIONS
*"      SQL_ERROR
*"      UNKOWN_ERROR
*"----------------------------------------------------------------------
  DATA: lv_client TYPE mandt,
        ltr_data  TYPE REF TO data,
        lv_offset TYPE i.
  FIELD-SYMBOLS: <lt_data> TYPE table.

  CLEAR: ev_json_zip, ev_count.

  CHECK: iv_tname IS NOT INITIAL.
  IF iv_client IS NOT INITIAL.
    lv_client = iv_client.
  ELSE.
    lv_client = sy-mandt.
  ENDIF.


  TRY .
      IF iv_flag_count_only IS NOT INITIAL.
        SELECT COUNT(*)
          FROM (iv_tname)
          USING CLIENT @lv_client
          INTO @ev_count
          WHERE (iv_where).
        RETURN.
      ENDIF.

      CREATE DATA ltr_data TYPE TABLE OF (iv_tname).
      ASSIGN ltr_data->* TO <lt_data>.

      IF iv_upto IS NOT INITIAL.
        lv_offset = iv_from - 1.
        SELECT *
          FROM (iv_tname)
          USING CLIENT @lv_client
          WHERE (iv_where)
          ORDER BY PRIMARY KEY
          INTO TABLE @<lt_data>
          UP TO @iv_upto ROWS OFFSET @lv_offset.
      ELSE.
        SELECT *
          FROM (iv_tname)
          USING CLIENT @lv_client
          WHERE (iv_where)
          INTO TABLE @<lt_data>.
      ENDIF.

      CHECK: <lt_data> IS NOT INITIAL.
      ev_count = lines( <lt_data> ).
      zcl_abap2json=>abap2json(
        EXPORTING
          it_data     = <lt_data>
          iv_where    = iv_where
        IMPORTING
          ev_json_zip = ev_json_zip
      ).

    CATCH cx_sy_dynamic_osql_error.
      RAISE sql_error.
    CATCH cx_root.
      RAISE unkown_error.
  ENDTRY.

ENDFUNCTION.
