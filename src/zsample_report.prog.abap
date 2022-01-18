*&---------------------------------------------------------------------*
*& Report ZSAMPLE_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsample_report.

PARAMETERS: p_fail TYPE abap_bool AS CHECKBOX.
PARAMETERS: p_cnt  TYPE i DEFAULT 5,
            p_wait TYPE i DEFAULT 10.

START-OF-SELECTION.
  DO p_cnt TIMES.
    WRITE: |Loop-Counter: { p_cnt NUMBER = USER }|.
    WAIT UP TO p_wait SECONDS.
  ENDDO.
  IF p_fail EQ abap_true.
    MESSAGE |Failing Report...| TYPE 'E'.
  ENDIF.
