*&---------------------------------------------------------------------*
*& Modulpool        Z_JOB_MONITOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
INCLUDE zjob_monitortop.

SELECTION-SCREEN BEGIN OF BLOCK bl_jb_cntr WITH FRAME TITLE TEXT-001.
PARAMETERS: p_jbcnt TYPE tbtcjob-jobcount OBLIGATORY,
            p_jbnam TYPE tbtcjob-jobname OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl_jb_cntr.

SELECTION-SCREEN BEGIN OF BLOCK bl_cntr WITH FRAME TITLE TEXT-002.
PARAMETERS: p_wait TYPE i DEFAULT 5.
SELECTION-SCREEN END OF BLOCK bl_cntr.

START-OF-SELECTION.
  DATA: job_status TYPE tbtcjob-status.
*  WRITE:/ |Monitoring of job: { p_jbnam } with Job-ID { p_jbcnt }|.
  MESSAGE: |Start Monitoring of job: { p_jbnam } with Job-ID { p_jbcnt }| TYPE 'I'.
  WHILE job_status <> zcl_job_base=>btc_finished AND job_status <> zcl_job_base=>btc_aborted.
    CALL FUNCTION 'BP_JOB_STATUS_GET'
      EXPORTING
        jobcount                   = p_jbcnt        " Kennummer eines Jobs
        jobname                    = p_jbnam        " Name eines Hintergrundjobs
*       read_only_status           =                 " Referenztyp CHAR1 für die Hintergrundverarbeitung
      IMPORTING
        status                     = job_status         " Zustand eines Batchjobs
*       has_child                  =                  " Flag: Job hat Kindjobs
      EXCEPTIONS
        job_doesnt_exist           = 1
        unknown_error              = 2
        parent_child_inconsistency = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    IF job_status = zcl_job_base=>btc_finished.
      "or write an email...
      WRITE:/ |job: { p_jbnam } with Job-ID { p_jbcnt } finished| COLOR COL_POSITIVE.
    ELSEIF job_status = zcl_job_base=>btc_aborted.
      "or write an email...
      WRITE:/ |job: { p_jbnam } with Job-ID { p_jbcnt } aborted| COLOR COL_NEGATIVE.
    ELSE.
      WAIT UP TO p_wait SECONDS.
    ENDIF.
  ENDWHILE.
