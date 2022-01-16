*&---------------------------------------------------------------------*
*& Modulpool        Z_SUBMIT_JOB_MONITOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
INCLUDE zsubmit_job_monitortop.

SELECTION-SCREEN BEGIN OF BLOCK bl_sel_par WITH FRAME TITLE TEXT-001.
PARAMETERS: p_jcnt TYPE tvarvc-name DEFAULT 'ZMONITOR_JOBCOUNT' OBLIGATORY,
            p_jnam TYPE tvarvc-name DEFAULT 'ZMONITOR_JOBNAME' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl_sel_par.

SELECTION-SCREEN BEGIN OF BLOCK bl_sel_job WITH FRAME TITLE TEXT-002.
PARAMETERS: p_jmsrc TYPE btcjob DEFAULT 'ZJOB_MONITOR' OBLIGATORY MATCHCODE OBJECT z_sh_planned_jobs.
PARAMETERS: p_jmtgt TYPE btcjob DEFAULT 'ZJOB_MONITOR_RUN'.
SELECTION-SCREEN END OF BLOCK bl_sel_job.

AT SELECTION-SCREEN ON p_jmsrc.
**********************************************************************
*** Check Copy Sources for Monitoring Job
  SELECT FROM tbtco AS job_def
       FIELDS COUNT( * ) AS cnt_cpy_srces
        WHERE job_def~jobname EQ @p_jmsrc
          AND job_def~status EQ @zcl_job_base=>btc_scheduled
         INTO @DATA(l_cnt).
  CASE l_cnt.
    WHEN 0.
      MESSAGE |Monitor-Job Copy Source not found!| TYPE 'E'.
    WHEN 1.
    WHEN OTHERS.
      MESSAGE |There are more than 1 Copy Sources| TYPE 'W'.
  ENDCASE.

START-OF-SELECTION.
*** Jobname and Jobcount (->Runtime-Info)
  DATA: jobname  TYPE tbtcm-jobname,
        jobcount TYPE tbtcm-jobcount.
  DATA: selpar_job_cnt  TYPE REF TO zcl_sel_par,
        selpar_job_name TYPE REF TO zcl_sel_par.
  DATA: monitor_job TYPE REF TO zcl_job_mon.
  DATA: enqueue_errors TYPE i.
  CONSTANTS: max_enqueue_errors TYPE i VALUE 5.
**********************************************************************
  IF sy-batch IS NOT INITIAL.
    CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
      IMPORTING
        jobcount        = jobcount
        jobname         = jobname
      EXCEPTIONS
        no_runtime_info = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE |Could not obtain Runtime-Info| TYPE 'E'.
    ENDIF.
**********************************************************************
*** Jobcount and Jobname are transferred via Selection-Variables
    TRY.
        selpar_job_cnt = zcl_sel_var_factory=>create_selpar_with_e_lock( p_jcnt ).
        selpar_job_cnt->set_value( jobcount ).
        selpar_job_cnt->save_db( ).
        selpar_job_name = zcl_sel_var_factory=>create_selpar_with_e_lock( p_jnam ).
        selpar_job_name->set_value( jobname ).
        selpar_job_name->save_db( ).
        COMMIT WORK.
      CATCH zcx_enqueue_error INTO DATA(lo_err).   " Kein Treffer bei DB-Abfrage
        enqueue_errors = enqueue_errors + 1.
        IF enqueue_errors <= max_enqueue_errors.
          WAIT UP TO 20 SECONDS.
          RETRY.
        ELSE.
          MESSAGE |Could not perform enqueue: { lo_err->get_text( ) }| TYPE 'E'.
        ENDIF.
    ENDTRY.
*** Copy Monitor-Job and release immediatly

    TRY.
        CREATE OBJECT monitor_job
          EXPORTING
            iv_job_src_name = p_jmsrc.
        "Copy Job to Default-Name
        IF p_jmtgt IS INITIAL.
          monitor_job->copy_to_target_job_dflt_name( ).
        ELSE.
          monitor_job->copy_to_target_job( iv_target_name = p_jmtgt iv_dialog = zcl_job_base=>z_btc_no ).
        ENDIF.
        monitor_job->release_target_job_now( ).
        WRITE:/ |Job-Monitor for Job-Name: { p_jmtgt } released|.
*** dequeue after monitor job is released
        TRY.
            TYPES: range_status TYPE RANGE OF c.
            WHILE monitor_job->get_state_of_target_job( )
                       IN VALUE range_status(
                                  ( option = 'EQ' sign = 'I' low = zcl_job_base=>btc_ready )
                                  ( option = 'EQ' sign = 'I' low = zcl_job_base=>btc_released ) ).
              WAIT UP TO 15 SECONDS.
            ENDWHILE.
          CATCH zcx_job_error. " zcx_job_error
        ENDTRY.
        selpar_job_cnt->dequeue_tvarv( ).
        selpar_job_name->dequeue_tvarv( ).
      CATCH zcx_job_error.
        MESSAGE |Could not release target Job for Job-Monitor| TYPE 'E'.
      CATCH zcx_no_entry_found. " zcx_no_entry_found
        MESSAGE |Copy-Source for Job-Monitor not found| TYPE 'E'.
    ENDTRY.
  ENDIF.
