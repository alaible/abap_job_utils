CLASS zcl_job_mon DEFINITION PUBLIC CREATE PUBLIC INHERITING FROM zcl_job_base.

  PUBLIC SECTION.
    METHODS: constructor
        IMPORTING iv_job_src_name TYPE tbtco-jobname iv_src_job_cnt TYPE tbtco-jobcount OPTIONAL
        RAISING   zcx_job_no_entry_found zcx_job_error.
protected section.
private section.
ENDCLASS.



CLASS ZCL_JOB_MON IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
*    super->constructor( iv_jobname = iv_job_src_name iv_status = btc_scheduled ).
    IF iv_src_job_cnt IS INITIAL.
      me->ms_job_cpy_source = me->read_newest_job( iv_jobname = iv_job_src_name iv_status = btc_scheduled ).
*       CATCH zcx_job_no_entry_found. " Allg. Fehlerklasse für nicht gef. Eintrag
    ELSE.
      DATA(job_res) = me->read_job( iv_jobname = iv_job_src_name iv_jobcount = iv_src_job_cnt iv_status = btc_scheduled ).
      IF lines( job_res ) NE 1.
        RAISE EXCEPTION TYPE zcx_job_error.
      ENDIF.
      me->ms_job_cpy_source = job_res[ 1 ].
    ENDIF.
  ENDMETHOD.
ENDCLASS.
