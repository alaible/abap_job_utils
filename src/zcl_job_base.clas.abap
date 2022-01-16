CLASS zcl_job_base DEFINITION PUBLIC CREATE PUBLIC .

  PUBLIC SECTION.
*** Public Typ-Definitionen
    TYPES: tty_tbtcjob TYPE TABLE OF tbtcjob WITH EMPTY KEY.
*** Allgemeine BTC-Konstanten
*** Konstanten für BTC_YES und BTC_NO
    CONSTANTS z_btc_no TYPE btch0000-char1 VALUE 'N' ##NO_TEXT.
    CONSTANTS z_btc_yes TYPE btch0000-char1 VALUE 'Y' ##NO_TEXT.
*** Konstanten JOB-Anzeige
    CONSTANTS btc_joblist_edit TYPE btch0000-int4 VALUE 21 ##NO_TEXT.
    CONSTANTS btc_joblist_show TYPE btch0000-int4 VALUE 22 ##NO_TEXT.
    CONSTANTS btc_joblist_select TYPE btch0000-int4 VALUE 23 ##NO_TEXT.
    CONSTANTS btc_job_edit TYPE btch0000-int4 VALUE 11 ##NO_TEXT.
    CONSTANTS btc_modify_whole_job TYPE btch0000-int4 VALUE 16 ##NO_TEXT.
    CONSTANTS btc_release_job TYPE btch0000-int4 VALUE 17 ##NO_TEXT.
*** Konstanten Job-Status
    CONSTANTS btc_running TYPE tbtco-status VALUE 'R' ##NO_TEXT.
    CONSTANTS btc_ready TYPE tbtco-status VALUE 'Y' ##NO_TEXT.
    CONSTANTS btc_scheduled TYPE tbtco-status VALUE 'P' ##NO_TEXT.
    CONSTANTS btc_intercepted TYPE btcstatus VALUE btc_scheduled ##NO_TEXT.
    CONSTANTS btc_released TYPE tbtco-status VALUE 'S' ##NO_TEXT.
    CONSTANTS btc_aborted TYPE tbtco-status VALUE 'A' ##NO_TEXT.
    CONSTANTS btc_finished TYPE tbtco-status VALUE 'F' ##NO_TEXT.
    CONSTANTS btc_put_active TYPE tbtco-status VALUE 'Z' ##NO_TEXT.
    CONSTANTS btc_unknown_state TYPE tbtco-status VALUE 'X' ##NO_TEXT.
*** Konstanten Start-Typ
    CONSTANTS btc_stdt_datetime TYPE tbtcstrt-startdttyp VALUE 'D' ##NO_TEXT.

    METHODS:
      set_target_name IMPORTING iv_target TYPE tbtcjob-jobname,
      release_target_job
        IMPORTING iv_dialog TYPE btch0000-char1
                  iv_date   TYPE dats
                  iv_time   TYPE tims
        RAISING   zcx_job_error,
      get_state_of_target_job
        RETURNING VALUE(rv_status) TYPE tbtcjob-status
        RAISING   zcx_job_error,
      release_target_job_now
        RAISING zcx_job_error,
      view_in_joblist_any_status
        IMPORTING iv_opcode   TYPE btch0000-int4
                  iv_jobname  TYPE tbtcjob-jobname
                  iv_jobcount TYPE tbtcjob-jobcount OPTIONAL
        RAISING   zcx_job_error zcx_job_no_entry_found,
      copy_to_target_job_dflt_name
        RAISING zcx_job_error,
      copy_to_target_job
        IMPORTING iv_target_name TYPE tbtcjob-jobname
                  iv_dialog      TYPE btch0000-char1
        RAISING   zcx_job_error.

  PROTECTED SECTION.
    METHODS:
      read_newest_job
        IMPORTING iv_jobname    TYPE tbtcjob-jobname
                  iv_status     TYPE tbtcjob-status
        RETURNING VALUE(rs_job) TYPE tbtcjob
        RAISING   zcx_job_no_entry_found,
      read_job
        IMPORTING iv_jobname        TYPE tbtcjob-jobname
                  iv_status         TYPE tbtcjob-status
                  iv_jobcount       TYPE tbtcjob-jobcount OPTIONAL
        RETURNING VALUE(rt_tbtcjob) TYPE tty_tbtcjob
        RAISING   zcx_job_no_entry_found,
      read_job_via_select
        IMPORTING is_job_select     TYPE btcselect
        RETURNING VALUE(rt_tbtcjob) TYPE tty_tbtcjob
        RAISING   zcx_job_no_entry_found,
      view_in_joblist
        IMPORTING iv_opcode   TYPE btch0000-int4
                  iv_status   TYPE tbtcjob-status
                  iv_jobname  TYPE tbtcjob-jobname
                  iv_jobcount TYPE tbtcjob-jobcount OPTIONAL
        RAISING   zcx_job_error zcx_job_no_entry_found.

    DATA: ms_job_cpy_source TYPE tbtcjob,
          ms_job_cpy_target TYPE tbtcjob.

    DATA: mv_target_name TYPE tbtcjob-jobname.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_JOB_BASE IMPLEMENTATION.


  METHOD copy_to_target_job.
    CALL FUNCTION 'BP_JOB_COPY'
      EXPORTING
        dialog                  = iv_dialog                  " Arbeitsmodus: Dialog ja / nein
        source_jobcount         = ms_job_cpy_source-jobcount " Job-Count des zu kopierenden Jobs
        source_jobname          = ms_job_cpy_source-jobname  " Job-Name des zu kopierenden Jobs
        target_jobname          = iv_target_name             " Job-Name der Job-Kopie (im Nichtdialogfall)
*       step_number             = 0                          " Kennummer eines Job-Steps
      IMPORTING
        new_jobhead             = ms_job_cpy_target  " Job-Count der Job-Kopie
*      CHANGING
*       ret                     =                  " Special additional error code
      EXCEPTIONS
        cant_create_new_job     = 1                " Kreieren des neuen Jobs ist mißlungen
        cant_enq_job            = 2                " Sperren des Quell-Jobs ist mißlungen
        cant_read_sourcedata    = 3                " Lesen des Quell-Jobs ist mißlungen
        invalid_opcode          = 4                " ungültiger Operationscode
        jobname_missing         = 5                " Name des Zieljobs fehlt
        job_copy_canceled       = 6                " Kopieren wurde vom Benutzer abgebrochen
        no_copy_privilege_given = 7                " Keine Berechtigung zum Kopieren des Source-Jobs
        no_plan_privilege_given = 8                " Keine Berechtigung zum Einplanen eines Jobs
        OTHERS                  = 9.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_error
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD copy_to_target_job_dflt_name.
    DATA: lv_target_dflt TYPE string.

*** Default-Jobname = Name Kopiervorlage - '_AB'
    lv_target_dflt = |{ ms_job_cpy_source-jobname }_AB|.
    IF strlen( lv_target_dflt ) > 32.
      lv_target_dflt = |{ ms_job_cpy_source-jobname(29) }_AB|.
    ENDIF.

*** Mit Zielname kopieren
    me->copy_to_target_job(
      EXPORTING
        iv_target_name = CONV #( lv_target_dflt )
        iv_dialog      = z_btc_no
    ).
*    CATCH zcx_job_error. " Allg. Fehler beim Job-Handling
  ENDMETHOD.


  METHOD get_state_of_target_job.
    CALL FUNCTION 'BP_JOB_STATUS_GET'
      EXPORTING
        jobcount                   = ms_job_cpy_target-jobcount       " Kennummer eines Jobs
        jobname                    = ms_job_cpy_target-jobname        " Name eines Hintergrundjobs
*       read_only_status           =                 " Referenztyp CHAR1 für die Hintergrundverarbeitung
      IMPORTING
        status                     = rv_status         " Zustand eines Batchjobs
*       has_child                  =                  " Flag: Job hat Kindjobs
      EXCEPTIONS
        job_doesnt_exist           = 1
        unknown_error              = 2
        parent_child_inconsistency = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_error
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD read_job.
*** tabelle um rückgabe aus fb aufzunehmen
    DATA: lt_jobselect TYPE TABLE OF tbtcjob,
          lt_jobname   TYPE TABLE OF njrange.
    DATA: ls_jobselect TYPE btcselect.

*** Selektionsbedingungen für Jobname
*    lt_jobname = VALUE #( ( sign = 'I' option = 'EQ' low = iv_jobname ) ).
    ls_jobselect = VALUE #( jobname = iv_jobname ).

*** Status Berücksichtigen
    CASE iv_status.
      WHEN btc_scheduled.
        ls_jobselect-prelim = abap_true.
      WHEN btc_finished.
        ls_jobselect-finished = abap_true.
      WHEN btc_running.
        ls_jobselect-running = abap_true.
      WHEN btc_ready.
        ls_jobselect-ready = abap_true.
      WHEN btc_aborted.
        ls_jobselect-aborted = abap_true.
    ENDCASE.

*** Job-Count in Selektionsbedingung aufnehmen (falls übergeben!)
    IF iv_jobcount IS NOT INITIAL.
      ls_jobselect-jobcount = iv_jobcount.
    ENDIF.

    rt_tbtcjob = me->read_job_via_select( ls_jobselect ).
*                 CATCH zcx_job_no_entry_found. " Allg. Fehlerklasse für nicht gef. Eintrag
  ENDMETHOD.


  METHOD read_job_via_select.
*** tabelle um rückgabe aus fb aufzunehmen
    DATA: lt_jobselect TYPE TABLE OF tbtcjob.

    CALL FUNCTION 'BP_JOB_SELECT'
      EXPORTING
        jobselect_dialog    = z_btc_no         " Dialog ja(Y) / nein(N)
        jobsel_param_in     = is_job_select    " Selektionsparameter (Input)
*       enddate             = '        '
*       endtime             = '      '
*       selection           = 'AL'             " Einstelliges Kennzeichen
*       adk_mode            = space            " Referenztyp CHAR1 für die Hintergrundverarbeitung
*       only_this_subsystem = 'Y'              " Referenztyp CHAR1 für die Hintergrundverarbeitung
*      IMPORTING
*       jobsel_param_out    =                  " Selektionsparameter (Output)
*       local_client        =                  " Referenztyp CHAR1 für die Hintergrundverarbeitung
*       nr_of_jobs_found    =
      TABLES
        jobselect_joblist   = lt_jobselect     " Selektierte Jobs
*       jobname_ext_sel     = lt_jobname        " Batch selection range for job name
*       username_ext_sel    =                  " Batch selection range for user name
*      CHANGING
*       error_code          =                  " Special additional error code
      EXCEPTIONS
        invalid_dialog_type = 1
        jobname_missing     = 2                " Jobname fehlt (Wildcards sind erlaubt)
        no_jobs_found       = 3                " kein Job entspricht den Selektionsbedingungen
        selection_canceled  = 4                " Benutzer bricht Selektion ab
        username_missing    = 5                " Benutzername fehlt (Wildcards erlaubt)
        OTHERS              = 6.
    CASE sy-subrc.
      WHEN 0.
        rt_tbtcjob = lt_jobselect.
*** Hier alles In Ordnung -> Selektions-Ergebnis zurückliefern
      WHEN 3.
        RAISE EXCEPTION TYPE zcx_job_no_entry_found.
    ENDCASE.
  ENDMETHOD.


  METHOD read_newest_job.
    DATA: lt_tab TYPE tty_tbtcjob.
    lt_tab = me->read_job( EXPORTING iv_jobname = iv_jobname iv_status = iv_status ).
*** Absteigend sortieren nach Job-Count -> neueste version
    SORT lt_tab BY sdldate DESCENDING sdltime DESCENDING.
    rs_job = lt_tab[ 1 ].
*** Exception wird durchgereicht
*    CATCH zcx_job_no_entry_found. " Allg. Fehlerklasse für nicht gef. Eintrag
  ENDMETHOD.


  METHOD release_target_job.
    DATA: lt_steplist TYPE TABLE OF tbtcstep.
    DATA: ls_release TYPE tbtcstrt.

*** Leere Liste als Dummy-Objekt hinzufügen
    lt_steplist = VALUE #( ( ) ).

*** Startdatum u.(!) Starttyp setzen (ohne Starttyp lässt sich der Job nicht ändern)
    ls_release-sdlstrtdt = CONV #( iv_date ).
    ls_release-sdlstrttm = CONV #( iv_time ).
    ls_release-startdttyp = btc_stdt_datetime.

    CALL FUNCTION 'BP_JOB_MODIFY'
      EXPORTING
        dialog                     = iv_dialog        " Arbeitsmodus des Fubst: Dialog ja / nein
        jobcount                   = ms_job_cpy_target-jobcount   " Kennummer des Jobs
        jobname                    = ms_job_cpy_target-jobname       " Jobname des zu modifizierenden Jobs
        opcode                     = btc_release_job  " Operation auf den Job: Ausplanen, freigeben, ...
        release_stdt               = ls_release       " Starttermin (nur bei Freig. im Nichtdialogfall)
*       direct_start               = 'X'                 " 'X' - Start ohne Umwandeln nach zeit-basierten Job
*       jobclass                   =                  " Klassifizierung von Jobs
*      IMPORTING
*       modified_jobhead           =                  " neue  Jobkopfdaten (Output, Dialog + Nichtdialog)
      TABLES
        new_steplist               = lt_steplist               " neue Stepliste
*      CHANGING
*       ret                        =                  " Special additional error code
*       emsg                       =                  " Fehlermeldung (nur bei Ausnahme)
      EXCEPTIONS
        cant_derelease_job         = 1                " Jobeinplanung kann nicht zurückgenommern werden
        cant_enq_job               = 2                " Job kann nicht gesperrt werden
        cant_read_jobdata          = 3                " Fehler beim Lesen der Jobdaten aus der DB
        cant_release_job           = 4                " Job kann nicht freigegeben werden
        cant_set_jobstatus_in_db   = 5                " Fehler beim Ändern des Jobstatus in DB
        cant_start_job_immediately = 6                " Sofortstart des Jobs misslungen
        cant_update_jobdata        = 7                " Jobdaten in DB können nicht upgedatet werden
        eventcnt_generation_error  = 8                " Generieren eines Eventcounts ist misslungen
        invalid_dialog_type        = 9
        invalid_new_jobdata        = 10               " neue Jobdaten sind ungültig
        invalid_new_jobstatus      = 11               " Status in neuen Jobdaten ungültig (Nichtdialog)
        invalid_opcode             = 12               " ungültiger Operationscode
        invalid_startdate          = 13
        job_edit_failed            = 14               " interner Fehler im Jobeditor
        job_modify_canceled        = 15               " Editieren von Jobdaten im Dialog wurde abgebr.
        job_not_modifiable_anymore = 16               " Job auf Grund seines Status nicht modifizierbar
        nothing_to_do              = 17               " gewünschter und akt. Status sind gleich
        no_batch_on_target_host    = 18               " Zielrchner hat keine Batchworkprozesse
        no_batch_server_found      = 19               " kein Server zur Abarbeitung des Jobs gefunden
        no_batch_wp_for_jobclass   = 20               " Jobklasse (B/C) auf Zielrechner nicht ausführbar
        no_modify_privilege_given  = 21               " Benutzer hat keine Änderungsberechtigung
        no_release_privilege_given = 22               " Benutzer hat keine Jobfreigabeberechtigung
        no_startdate_no_release    = 23               " ohne Starttermin keine Freigabe möglich
        target_host_not_defined    = 24               " Zielrechner in keiner Betriebsart definiert
        tgt_host_chk_has_failed    = 25               " Zielrechnerprüfung ist misslungen
        invalid_targetgroup        = 26               " Ungueltige Zielserver-Gruppe
        conflicting_targets        = 27               " Zielserver und Zielserver-Gruppe nicht gleichzeitig angeben
        OTHERS                     = 28.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_error
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD release_target_job_now.
    me->release_target_job(
      EXPORTING
        iv_dialog = z_btc_no
        iv_date   = sy-datum
        iv_time   = sy-uzeit
    ).
*    CATCH zcx_job_error. " Allg. Fehler beim Job-Handling
  ENDMETHOD.


  METHOD set_target_name.
    me->mv_target_name = iv_target.
  ENDMETHOD.


  METHOD view_in_joblist.
    DATA: lt_joblist   TYPE TABLE OF tbtcjob,
          lt_joblist_b TYPE TABLE OF tbtcjob_bk,
          ls_jobselect TYPE btcselect.

*** Username wird hier noch nicht gesetzt!
    ls_jobselect = VALUE #(
                            jobname = iv_jobname
                            jobcount = iv_jobcount
                            username = '*'
                            no_date = z_btc_yes ).
*** Status Berücksichtigen
    CASE iv_status.
      WHEN btc_scheduled.
        ls_jobselect-prelim = abap_true.
      WHEN btc_finished.
        ls_jobselect-finished = abap_true.
      WHEN btc_running.
        ls_jobselect-running = abap_true.
      WHEN btc_ready.
        ls_jobselect-ready = abap_true.
      WHEN btc_aborted.
        ls_jobselect-aborted = abap_true.
    ENDCASE.


    lt_joblist = me->read_job_via_select(
                   ls_jobselect
                 ).
*** Selektion des Jobs über FB BP_JOB_SELECT
*** -> der FB BP_JOBLIST_PROCESSOR_SM37B nimmt die Ergebnisliste als Parameter entgegen
    CALL FUNCTION 'BP_JOBLIST_PROCESSOR_SM37B'
      EXPORTING
        joblist_opcode             = iv_opcode      " Arbeitsmodus Anzeigen / Editieren
        joblist_refr_param         = ls_jobselect   " Selektionsparameter für 'Auffrischen' Jobliste
*       exit_at_refresh            = space
*            IMPORTING
*       joblist_sel_job            =                " Der aus der Liste selektierte Job
      TABLES
        joblist                    = lt_joblist     " Jobliste
      EXCEPTIONS
        invalid_opcode             = 1                " Falscher Wert für den Parameter JOBLIST_OPCODE
        joblist_is_empty           = 2                " Keine Jobs wurden gefunden
        joblist_processor_canceled = 3                " Joblistenprozessor wurde abgebrochen
        refresh_list_required      = 4                " Auffrischen und EXIT_AT_REFRESH war gesetzt
        OTHERS                     = 5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_error
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD view_in_joblist_any_status.
    DATA: lt_joblist   TYPE TABLE OF tbtcjob,
          lt_joblist_b TYPE TABLE OF tbtcjob_bk,
          ls_jobselect TYPE btcselect.

*** Username wird hier noch nicht gesetzt!
    ls_jobselect = VALUE #(
                            jobname = iv_jobname
                            jobcount = iv_jobcount
                            username = '*'
                            no_date = z_btc_yes
                            finished = abap_true
                            running = abap_true
                            ready = abap_true
                            schedul = abap_true
                            prelim = abap_true
                            aborted = abap_true
                          ).

    lt_joblist = me->read_job_via_select(
                   ls_jobselect
                 ).
*** Selektion des Jobs über FB BP_JOB_SELECT
*** -> der FB BP_JOBLIST_PROCESSOR_SM37B nimmt die Ergebnisliste als Parameter entgegen
    CALL FUNCTION 'BP_JOBLIST_PROCESSOR_SM37B'
      EXPORTING
        joblist_opcode             = iv_opcode      " Arbeitsmodus Anzeigen / Editieren
        joblist_refr_param         = ls_jobselect   " Selektionsparameter für 'Auffrischen' Jobliste
*       exit_at_refresh            = space
*            IMPORTING
*       joblist_sel_job            =                " Der aus der Liste selektierte Job
      TABLES
        joblist                    = lt_joblist     " Jobliste
      EXCEPTIONS
        invalid_opcode             = 1                " Falscher Wert für den Parameter JOBLIST_OPCODE
        joblist_is_empty           = 2                " Keine Jobs wurden gefunden
        joblist_processor_canceled = 3                " Joblistenprozessor wurde abgebrochen
        refresh_list_required      = 4                " Auffrischen und EXIT_AT_REFRESH war gesetzt
        OTHERS                     = 5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_error
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
