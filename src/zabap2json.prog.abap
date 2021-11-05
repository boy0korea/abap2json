* https://github.com/boy0korea/abap2json
REPORT zabap2json.


**********************************************************************
* SCREEN
**********************************************************************
PARAMETERS: p_impo   TYPE flag RADIOBUTTON GROUP rd DEFAULT 'X' USER-COMMAND rd,
            p_expo_t TYPE flag RADIOBUTTON GROUP rd,
            p_expo_p TYPE flag RADIOBUTTON GROUP rd,
            p_comp   TYPE flag RADIOBUTTON GROUP rd,
            p_copy   TYPE flag RADIOBUTTON GROUP rd.

SELECTION-SCREEN ULINE.
PARAMETERS: p_folder TYPE string LOWER CASE MODIF ID 1,
            p_packag TYPE tadir-devclass MODIF ID 2,
            p_table  TYPE info_tabl-tabname MATCHCODE OBJECT dd_dbtb_16 MODIF ID 3,
            p_where  TYPE string LOWER CASE MODIF ID 4,
            p_server TYPE char30 AS LISTBOX VISIBLE LENGTH 50 MODIF ID 6,
            p_serve2 TYPE char30 AS LISTBOX VISIBLE LENGTH 50 MODIF ID 7,
            p_del    TYPE flag MODIF ID 8,
            p_simul  TYPE flag MODIF ID 9.


**********************************************************************
AT SELECTION-SCREEN OUTPUT.
**********************************************************************
  CASE abap_true.
    WHEN p_impo.
      LOOP AT SCREEN.
        CHECK: screen-group1 IS NOT INITIAL.
        CASE screen-group1.
          WHEN 1 OR 6 OR 8 OR 9.
            screen-active = 1.
          WHEN OTHERS.
            screen-active = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN p_expo_t.
      LOOP AT SCREEN.
        CHECK: screen-group1 IS NOT INITIAL.
        CASE screen-group1.
          WHEN 1 OR 3 OR 4 OR 6.
            screen-active = 1.
          WHEN OTHERS.
            screen-active = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN p_expo_p.
      LOOP AT SCREEN.
        CHECK: screen-group1 IS NOT INITIAL.
        CASE screen-group1.
          WHEN 1 OR 2 OR 6.
            screen-active = 1.
          WHEN OTHERS.
            screen-active = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN p_comp.
      LOOP AT SCREEN.
        CHECK: screen-group1 IS NOT INITIAL.
        CASE screen-group1.
          WHEN 3 OR 4 OR 6 OR 7.
            screen-active = 1.
          WHEN OTHERS.
            screen-active = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN p_copy.
      LOOP AT SCREEN.
        CHECK: screen-group1 IS NOT INITIAL.
        CASE screen-group1.
          WHEN 3 OR 4 OR 6 OR 7 OR 8 OR 9.
            screen-active = 1.
          WHEN OTHERS.
            screen-active = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
  ENDCASE.


**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_folder.
**********************************************************************
  PERFORM select_folder USING p_folder.

**********************************************************************
INITIALIZATION.
**********************************************************************
  PERFORM do_init.

**********************************************************************
START-OF-SELECTION.
**********************************************************************
  DATA: lt_log        TYPE zcl_abap2json=>tt_log,
        lv_error_text TYPE text255,
        lv_rfcdest    TYPE rfcdest,
        lv_client     TYPE mandt,
        lv_rfcdest2   TYPE rfcdest,
        lv_client2    TYPE mandt,
        lv_same       TYPE flag.

  CASE abap_true.
    WHEN p_impo.
      zcl_abap2json=>get_server_info(
        EXPORTING
          iv_server  = p_server
        IMPORTING
          ev_rfcdest = lv_rfcdest
          ev_client  = lv_client
      ).
      zcl_abap2json=>import_json_zip(
        EXPORTING
          iv_folder            = p_folder
          iv_del               = p_del
          iv_simulate          = p_simul
          iv_show_progress_bar = abap_true
          iv_rfcdest           = lv_rfcdest
          iv_client            = lv_client
        IMPORTING
          et_log               = lt_log
          ev_error_text        = lv_error_text
      ).
    WHEN p_expo_t.
      CHECK: p_table IS NOT INITIAL.
      zcl_abap2json=>get_server_info(
        EXPORTING
          iv_server  = p_server
        IMPORTING
          ev_rfcdest = lv_rfcdest
          ev_client  = lv_client
      ).
      zcl_abap2json=>export_table(
        EXPORTING
          iv_table             = p_table
          iv_folder            = p_folder
          iv_show_progress_bar = abap_true
          iv_where             = p_where
        IMPORTING
          et_log               = lt_log
          ev_error_text        = lv_error_text
      ).
    WHEN p_expo_p.
      CHECK: p_packag IS NOT INITIAL.
      zcl_abap2json=>get_server_info(
        EXPORTING
          iv_server  = p_server
        IMPORTING
          ev_rfcdest = lv_rfcdest
          ev_client  = lv_client
      ).
      zcl_abap2json=>export_package(
        EXPORTING
          iv_package           = p_packag
          iv_folder            = p_folder
          iv_show_progress_bar = abap_true
        IMPORTING
          et_log               = lt_log
          ev_error_text        = lv_error_text
      ).
    WHEN p_comp.
      CHECK: p_table IS NOT INITIAL.
      zcl_abap2json=>get_server_info(
        EXPORTING
          iv_server  = p_server
        IMPORTING
          ev_rfcdest = lv_rfcdest
          ev_client  = lv_client
      ).
      zcl_abap2json=>get_server_info(
        EXPORTING
          iv_server  = p_serve2
        IMPORTING
          ev_rfcdest = lv_rfcdest2
          ev_client  = lv_client2
      ).
      zcl_abap2json=>compare_table(
        EXPORTING
          iv_table             = p_table
          iv_where             = p_where
          iv_show_progress_bar = abap_true
          iv_rfcdest           = lv_rfcdest
          iv_client            = lv_client
          iv_rfcdest2          = lv_rfcdest2
          iv_client2           = lv_client2
        IMPORTING
          ev_same              = lv_same
          ev_error_text        = lv_error_text
      ).
      IF lv_same EQ abap_true.
        MESSAGE 'Same' TYPE 'S'.
      ELSE.
        MESSAGE 'Different' TYPE 'S'.
      ENDIF.
    WHEN p_copy.
      CHECK: p_table IS NOT INITIAL.
      zcl_abap2json=>get_server_info(
        EXPORTING
          iv_server  = p_server
        IMPORTING
          ev_rfcdest = lv_rfcdest
          ev_client  = lv_client
      ).
      zcl_abap2json=>get_server_info(
        EXPORTING
          iv_server  = p_serve2
        IMPORTING
          ev_rfcdest = lv_rfcdest2
          ev_client  = lv_client2
      ).
      zcl_abap2json=>copy_table(
        EXPORTING
          iv_table             = p_table
          iv_where             = p_where
          iv_del               = p_del
          iv_simulate          = p_simul
          iv_show_progress_bar = abap_true
          iv_rfcdest           = lv_rfcdest
          iv_client            = lv_client
          iv_rfcdest2          = lv_rfcdest2
          iv_client2           = lv_client2
        IMPORTING
          et_log               = lt_log
          ev_error_text        = lv_error_text
      ).
  ENDCASE.

  IF lv_error_text IS NOT INITIAL.
    MESSAGE lv_error_text TYPE 'E'.
  ENDIF.

  IF lt_log IS NOT INITIAL.
    cl_demo_output=>display( lt_log ).
  ENDIF.

END-OF-SELECTION.


**********************************************************************
* form
**********************************************************************
FORM do_init.
  DATA: lt_dd   TYPE vrm_values.

  lt_dd = zcl_abap2json=>init_server_info( ).

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_SERVER'
      values = lt_dd.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_SERVE2'
      values = lt_dd.

ENDFORM.
FORM select_folder USING cv_folder.
  DATA: lt_df TYPE TABLE OF dynpread,
        ls_df TYPE dynpread.

  ls_df-fieldname = 'P_FOLDER'.
  APPEND ls_df TO lt_df.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_df.
  cv_folder = lt_df[ 1 ]-fieldvalue.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      initial_folder       = cv_folder
    CHANGING
      selected_folder      = cv_folder " Folder Selected By User
    EXCEPTIONS
      cntl_error           = 1               " Control error
      error_no_gui         = 2               " No GUI available
      not_supported_by_gui = 3               " GUI does not support this
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
