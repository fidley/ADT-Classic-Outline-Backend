CLASS zcl_adtco_tree_creator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: create_tree IMPORTING object_name TYPE eu_lname
                                   object_type TYPE seu_obj
                         RETURNING VALUE(tree) TYPE snodetab.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_object_name
      IMPORTING
        original_object_name TYPE eu_lname
        original_object_type TYPE seu_obj
      RETURNING
        VALUE(object_name)   TYPE eu_lname.
    METHODS get_object_type
      IMPORTING
        original_object_type TYPE seu_obj
      RETURNING
        VALUE(object_type)   TYPE seu_obj.
    METHODS add_sublcasses
      IMPORTING original_object_name TYPE eu_lname
                original_object_type TYPE seu_obj
      CHANGING  VALUE(tree)          TYPE snodetab.
    METHODS get_class_description
      IMPORTING
        class_name         TYPE seorelkey-clsname
      RETURNING
        VALUE(description) TYPE char72.
    METHODS get_counter
      IMPORTING
        tree           TYPE snodetab
      RETURNING
        VALUE(counter) TYPE i.
    METHODS get_subclasses
      IMPORTING
        class_name        TYPE string
      RETURNING
        VALUE(subclasses) TYPE seo_relkeys.
    METHODS actualize_program_tree
      IMPORTING
        object_type      TYPE seu_obj
        object_name      TYPE eu_lname
        tree_object_type TYPE seu_obj.
ENDCLASS.



CLASS zcl_adtco_tree_creator IMPLEMENTATION.


  METHOD add_sublcasses.
    CHECK original_object_type EQ 'CLAS/OC'.

    ASSIGN tree[ type = 'COU' ] TO FIELD-SYMBOL(<parent>).
    IF sy-subrc EQ 0.
      DATA(counter) = get_counter( tree ).
      DATA(subclasses) = get_subclasses( CONV #( original_object_name ) ).
      LOOP AT subclasses ASSIGNING FIELD-SYMBOL(<subclass>).
        IF <parent>-child IS INITIAL.
          <parent>-child = counter.
        ENDIF.
        APPEND VALUE #( text1 = <subclass>-clsname parent = <parent>-id id = counter type = 'OOC' text2 = get_class_description( <subclass>-clsname )  ) TO tree.
        ADD 1 TO counter.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD create_tree.
    DATA(tree_object_type) = get_object_type( object_type ).
    actualize_program_tree(  object_type      = object_type
                             object_name      = object_name
                             tree_object_type = tree_object_type ).
    CALL FUNCTION 'WB_ANYTYPE_RETURN_OBJECT_LIST'
      EXPORTING
        p_object_type        = tree_object_type
        p_object_name        = CONV eu_lname( get_object_name( original_object_name = object_name
                                                             original_object_type = object_type ) )
      TABLES
        nodetab              = tree
      EXCEPTIONS
        objectlist_not_found = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      IF object_type EQ 'REPS'.
        CALL FUNCTION 'WB_TREE_RETURN_OBJECT_LIST'
          EXPORTING
            treename     = CONV eu_t_name( |PG_{ object_name }| )
            refresh      = 'X'
          TABLES
            nodetab      = tree
          EXCEPTIONS
            not_existing = 1
            OTHERS       = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
    ENDIF.
    add_sublcasses( EXPORTING original_object_name = object_name
                              original_object_type = object_type
                    CHANGING  tree                 = tree ).
  ENDMETHOD.

  METHOD actualize_program_tree.

    IF tree_object_type CP 'PROG*'
    OR tree_object_type CP 'REPS*'
    OR tree_object_type CP 'FUGR*'.
      CALL FUNCTION 'WB_TREE_ACTUALIZE'
        EXPORTING
          tree_name              = CONV eu_lname( |PG_{ get_object_name( original_object_name = object_name
                                                             original_object_type = object_type ) }| )
          without_crossreference = abap_true
          with_tcode_index       = abap_true.
    ELSEIF tree_object_type CP 'CLAS*' OR
           tree_object_type CP 'INTF*'.
      CALL FUNCTION 'WB_TREE_ACTUALIZE'
        EXPORTING
          tree_name              = CONV eu_lname( |CP_{ object_name }| )
          without_crossreference = abap_true
          with_tcode_index       = abap_true.
    ENDIF.

  ENDMETHOD.




  METHOD get_class_description.
    TRY.
        DATA(class) = CAST cl_oo_class( cl_oo_class=>get_instance( class_name ) ).
        description = class->class-descript.
      CATCH cx_class_not_existent ##no_handler.
    ENDTRY.
  ENDMETHOD.


  METHOD get_counter.
    counter = tree[ lines( tree ) ]-id + 1.
  ENDMETHOD.


  METHOD get_object_name.
    CASE original_object_type.
      WHEN 'FUGR/FF'.
        SELECT SINGLE pname FROM tfdir
        INTO object_name
        WHERE funcname = original_object_name.
        REPLACE FIRST OCCURRENCE OF 'SAPL' IN object_name WITH ''.
      WHEN 'FUGR/I'.
        object_name = original_object_name.
        IF object_name(1) EQ '/'.
          DATA(regex) = NEW cl_abap_regex(   pattern       =  '(\/.*\/)L(.*)' ).
          DATA(matcher) = regex->create_matcher( text = object_name ).
          IF matcher->match( ).
            object_name = |{ matcher->get_submatch( index = 1 ) }{ matcher->get_submatch( index = 2 ) }|.
          ENDIF.
        ELSE.
          SHIFT object_name BY 1 PLACES LEFT.
        ENDIF.
        DATA(lenght) = strlen( object_name ) - 3.
        object_name = object_name(lenght).
      WHEN 'REPS' OR 'PROG/I'.
        SELECT SINGLE master INTO @object_name
          FROM d010inc
          WHERE include EQ @original_object_name.
        object_name+40 = original_object_name.
      WHEN OTHERS.
        object_name = original_object_name.
    ENDCASE.
  ENDMETHOD.


  METHOD get_object_type.
    object_type = original_object_type(4).
  ENDMETHOD.


  METHOD get_subclasses.
    subclasses =  CAST cl_oo_class( cl_oo_class=>get_instance( CONV #( class_name ) ) )->get_subclasses( ).
    SORT subclasses BY clsname.
  ENDMETHOD.
ENDCLASS.
