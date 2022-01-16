*&---------------------------------------------------------------------*
*& Modulpool        Z_JOB_MONITOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
INCLUDE zjob_monitortop.

SELECTION-SCREEN BEGIN OF BLOCK bl_jb_cntr WITH FRAME TITLE TEXT-001.
PARAMETERS: pa_jbcnt TYPE tbtcjob-jobcount OBLIGATORY,
            pa_jbnam TYPE tbtcjob-jobname OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl_jb_cntr.

SELECTION-SCREEN BEGIN OF BLOCK bl_cntr WITH FRAME TITLE TEXT-002.
PARAMETERS: pa_wait TYPE i DEFAULT 5.
SELECTION-SCREEN END OF BLOCK bl_cntr.

START-OF-SELECTION.
  DATA: lv_status TYPE tbtcjob-status.
  WRITE:/ |Monitoring of job: { pa_jbnam } with Job-Count { pa_jbcnt }|.
  MESSAGE: |Monitoring of job: { pa_jbnam } with Job-Count { pa_jbcnt }| TYPE 'I'.
  WHILE lv_status <> 'F' AND lv_status <> 'A'.
    CALL FUNCTION 'BP_JOB_STATUS_GET'
      EXPORTING
        jobcount                   = pa_jbcnt        " Kennummer eines Jobs
        jobname                    = pa_jbnam        " Name eines Hintergrundjobs
*       read_only_status           =                 " Referenztyp CHAR1 für die Hintergrundverarbeitung
      IMPORTING
        status                     = lv_status         " Zustand eines Batchjobs
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
    WRITE:/ |Job-Status Time({ sy-uzeit TIME = USER }): { lv_status }|.
    MESSAGE: |Job-Status Time({ sy-uzeit TIME = USER }): { lv_status }| TYPE 'I'.
    IF lv_status = 'F'.
      "raise event job_finished...
    ELSEIF lv_status = 'A'.
      "raise event job_abbruch...
    ELSE.
      WAIT UP TO pa_wait SECONDS.
    ENDIF.
  ENDWHILE.
* INCLUDE Z_JOB_MONITORO01                        .  " PBO-Modules
* INCLUDE Z_JOB_MONITORI01                        .  " PAI-Modules
* INCLUDE Z_JOB_MONITORF01                        .  " FORM-Routines
