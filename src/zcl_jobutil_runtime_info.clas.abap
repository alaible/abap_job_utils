CLASS zcl_jobutil_runtime_info DEFINITION PUBLIC CREATE PUBLIC .
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_runtime_info,
             job_name   TYPE tbtco-jobname,
             job_count  TYPE tbtco-jobcount,
             start_date TYPE dats,
             start_time TYPE tims,
             start_ts   TYPE i,
             end_ts     TYPE i,
             status     TYPE tbtco-status,
           END OF ty_runtime_info.
    METHODS:
      read_runtime_info RAISING zcx_job_error,
      set_start_time,
      set_end_time,
      get_runtime_info RETURNING VALUE(rs_runtime_info) TYPE ty_runtime_info,
      get_exec_time_in_s RETURNING VALUE(rv_exec_time) TYPE i.
  PROTECTED SECTION.
*** Jobname und Jobkennung
    DATA: ms_runtime_info TYPE ty_runtime_info.
    DATA: mv_start_ts TYPE timestamp,
          mv_end_ts   TYPE timestamp.
private section.
ENDCLASS.



CLASS ZCL_JOBUTIL_RUNTIME_INFO IMPLEMENTATION.


  METHOD get_exec_time_in_s.
    TRY.
        DATA(lv_sec) = cl_abap_tstmp=>subtract(
          EXPORTING
            tstmp1 = mv_end_ts        " UTC-Zeitstempel
            tstmp2 = mv_start_ts      " UTC-Zeitstempel
        ).
        rv_exec_time = CONV #( lv_sec ).
      CATCH cx_parameter_invalid_range. " Parameter mit ungültigem Wertebereich
      CATCH cx_parameter_invalid_type.  " Parameter mit ungültigem Typ
    ENDTRY.
  ENDMETHOD.


  METHOD get_runtime_info.
    rs_runtime_info = ms_runtime_info.
  ENDMETHOD.


  METHOD read_runtime_info.
    CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
      IMPORTING
*       eventid         =
*       eventparm       =
*       external_program_active =
        jobcount        = ms_runtime_info-job_count
        jobname         = ms_runtime_info-job_name
*       stepcount       =
      EXCEPTIONS
        no_runtime_info = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_error
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD set_end_time.
    GET RUN TIME FIELD ms_runtime_info-end_ts.
    GET TIME STAMP FIELD mv_end_ts.
  ENDMETHOD.


  METHOD set_start_time.
    ms_runtime_info-start_date = sy-datum.
    ms_runtime_info-start_time = sy-uzeit.
    GET RUN TIME FIELD ms_runtime_info-start_ts.
    GET TIME STAMP FIELD mv_start_ts.
  ENDMETHOD.
ENDCLASS.
