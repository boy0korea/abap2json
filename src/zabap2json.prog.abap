*&---------------------------------------------------------------------*
*& Report ZABAP2JSON
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap2json.


**********************************************************************
* SCREEN
**********************************************************************
PARAMETERS: p_impo_z TYPE flag RADIOBUTTON GROUP rd DEFAULT 'X' USER-COMMAND rd,
            p_comp_z TYPE flag RADIOBUTTON GROUP rd,
            p_expo_t TYPE flag RADIOBUTTON GROUP rd,
            p_expo_p TYPE flag RADIOBUTTON GROUP rd,
            p_comp_t TYPE flag RADIOBUTTON GROUP rd,
            p_copy_t TYPE flag RADIOBUTTON GROUP rd.

SELECTION-SCREEN ULINE.
PARAMETERS: p_folder TYPE string LOWER CASE MEMORY ID folder MODIF ID 1,
            p_packag TYPE tadir-devclass MODIF ID 2,
            p_table  TYPE info_tabl-tabname MEMORY ID dtb MATCHCODE OBJECT dd_dbtb_16 MODIF ID 3,
            p_where  TYPE string LOWER CASE MODIF ID 4,
            p_server TYPE char30 AS LISTBOX VISIBLE LENGTH 50 MODIF ID 6,
            p_serve2 TYPE char30 AS LISTBOX VISIBLE LENGTH 50 MODIF ID 7,
            p_del    TYPE flag MODIF ID 8,
            p_simul  TYPE flag MODIF ID 9.


**********************************************************************
AT SELECTION-SCREEN OUTPUT.
**********************************************************************
  PERFORM loop_screen.

**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_folder.
**********************************************************************
  PERFORM f4_folder.

**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_where.
**********************************************************************
  PERFORM f4_where.

**********************************************************************
INITIALIZATION.
**********************************************************************
  PERFORM do_init.

**********************************************************************
START-OF-SELECTION.
**********************************************************************
  PERFORM execute.


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
FORM loop_screen.
  CASE abap_true.
    WHEN p_impo_z.
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
    WHEN p_comp_z.
      LOOP AT SCREEN.
        CHECK: screen-group1 IS NOT INITIAL.
        CASE screen-group1.
          WHEN 1 OR 6.
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
    WHEN p_comp_t.
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
    WHEN p_copy_t.
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
ENDFORM.
FORM f4_folder.
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
  READ TABLE lt_df INTO ls_df INDEX 1.
  p_folder = ls_df-fieldvalue.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      initial_folder       = p_folder
    CHANGING
      selected_folder      = p_folder " Folder Selected By User
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
FORM f4_where.
  DATA: lt_df     TYPE TABLE OF dynpread,
        ls_df     TYPE dynpread,
        lt_return TYPE TABLE OF ddshretval,
        ls_return TYPE ddshretval.

  ls_df-fieldname = 'P_WHERE'.
  APPEND ls_df TO lt_df.
  ls_df-fieldname = 'P_TABLE'.
  APPEND ls_df TO lt_df.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_df.
  READ TABLE lt_df INTO ls_df INDEX 1.
  p_where = ls_df-fieldvalue.
  READ TABLE lt_df INTO ls_df INDEX 2.
  p_table = ls_df-fieldvalue.

  SET PARAMETER ID 'DTB' FIELD p_table.
  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname           = ''         " Table/structure name from Dictionary
      fieldname         = ''       " Field name from Dictionary
      searchhelp        = 'EDOC_SH_TABLE_FIELDNAME'           " Search help as screen field attribute
      multiple_choice   = abap_true           " Switch on multiple selection
    TABLES
      return_tab        = lt_return      " Return the selected value
    EXCEPTIONS
      field_not_found   = 1               " Field does not exist in the Dictionary
      no_help_for_field = 2               " No F4 help is defined for the field
      inconsistent_help = 3               " F4 help for the field is inconsistent
      no_values_found   = 4               " No values found
      OTHERS            = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_return INTO ls_return.
    IF p_where IS INITIAL.
      p_where = ls_return-fieldval.
    ELSE.
      CONCATENATE p_where ls_return-fieldval INTO p_where SEPARATED BY space.
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM execute.
  DATA: lt_log        TYPE zcl_abap2json=>tt_log,
        lv_error_text TYPE text255,
        lv_rfcdest    TYPE rfcdest,
        lv_client     TYPE mandt,
        lv_rfcdest2   TYPE rfcdest,
        lv_client2    TYPE mandt,
        lv_same       TYPE flag.

  CASE abap_true.
    WHEN p_impo_z.
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
    WHEN p_comp_z.
      zcl_abap2json=>get_server_info(
        EXPORTING
          iv_server  = p_server
        IMPORTING
          ev_rfcdest = lv_rfcdest
          ev_client  = lv_client
      ).
      zcl_abap2json=>compare_json_zip(
        EXPORTING
          iv_folder            = p_folder
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
    WHEN p_comp_t.
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
    WHEN p_copy_t.
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
    MESSAGE lv_error_text TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF lt_log IS NOT INITIAL.
    cl_demo_output=>display( lt_log ).
  ENDIF.

ENDFORM.
