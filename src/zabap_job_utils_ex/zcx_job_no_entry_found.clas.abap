class ZCX_JOB_NO_ENTRY_FOUND definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .
  interfaces IF_T100_DYN_MSG .

  constants:
    begin of ZCX_JOB_NO_ENTRY_FOUND,
      msgid type symsgid value 'ZJOB_ERR',
      msgno type symsgno value '002',
      attr1 type scx_attrname value 'MV_OBJ',
      attr2 type scx_attrname value 'MV_KEY',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_JOB_NO_ENTRY_FOUND .
  data MV_OBJ type SYMSGV .
  data MV_KEY type SYMSGV .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_OBJ type SYMSGV optional
      !MV_KEY type SYMSGV optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_JOB_NO_ENTRY_FOUND IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MV_OBJ = MV_OBJ .
me->MV_KEY = MV_KEY .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_JOB_NO_ENTRY_FOUND .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
