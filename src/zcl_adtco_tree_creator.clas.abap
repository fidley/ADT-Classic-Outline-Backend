CLASS zcl_adtco_tree_creator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_tree TYPE STANDARD TABLE OF snodetext WITH DEFAULT KEY
                                                    WITH NON-UNIQUE SORTED KEY type COMPONENTS type
                                                    WITH NON-UNIQUE SORTED KEY method COMPONENTS text1 type.
    TYPES: BEGIN OF t_attribute,
             clsname    TYPE seocompo-clsname,
             cmpname    TYPE seocompo-cmpname,
             version    TYPE seocompodf-version,
             attrdonly  TYPE seocompodf-attrdonly,
             mtdabstrct TYPE seocompodf-mtdabstrct,
             mtdfinal   TYPE seocompodf-mtdfinal,
             mtddecltyp TYPE seocompodf-mtddecltyp,
             attdecltyp TYPE seocompodf-attdecltyp,
             exposure   TYPE seocompodf-exposure,
             typtype    TYPE seocompodf-typtype,
             refcmpname TYPE seocompodf-refcmpname,
             cmptype    TYPE seocompo-cmptype,
             mtdtype    TYPE seocompo-mtdtype,
           END OF t_attribute.
    METHODS create_tree
      IMPORTING
        !object_name      TYPE eu_lname
        !object_type      TYPE seu_obj
        VALUE(parameters) TYPE tihttpnvp OPTIONAL
      RETURNING
        VALUE(tree)       TYPE tt_tree .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS class_type TYPE string VALUE 'CLAS/OC' ##NO_TEXT.
    CONSTANTS interface_type TYPE string VALUE 'INTF/OI' ##NO_TEXT.
    TYPES: BEGIN OF ty_parameters,
             load_redefinitions_of_method  TYPE abap_bool,
             load_all_levels_of_subclasses TYPE abap_bool,
             load_all_levels_of_redef      TYPE abap_bool,
           END OF ty_parameters.
    TYPES:
      ty_class_names TYPE RANGE OF seocompo-clsname.
    METHODS get_subclasses_description
      IMPORTING
        original_object_name TYPE eu_lname.
    METHODS build_class_range
      IMPORTING
        original_object_name TYPE eu_lname
      RETURNING
        VALUE(class_names)   TYPE ty_class_names.
    DATA relations TYPE REF TO cl_oo_class_relations.
    DATA subclasses_descriptions TYPE HASHED TABLE OF seoclasstx WITH UNIQUE KEY clsname.
    DATA call_parameters TYPE ty_parameters.
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
      CHANGING  VALUE(tree)          TYPE tt_tree.
    METHODS get_class_description
      IMPORTING
        class_name         TYPE seorelkey-clsname
      RETURNING
        VALUE(description) TYPE char72.
    METHODS get_counter
      IMPORTING
        tree           TYPE tt_tree
      RETURNING
        VALUE(counter) TYPE i.
    METHODS get_subclasses
      IMPORTING
        object_name       TYPE eu_lname
      RETURNING
        VALUE(subclasses) TYPE seo_relkeys.
    METHODS actualize_program_tree
      IMPORTING
        object_type      TYPE seu_obj
        object_name      TYPE eu_lname
        tree_object_type TYPE seu_obj.
    METHODS add_superclasses
      IMPORTING
        original_object_name TYPE eu_lname
        original_object_type TYPE seu_obj
      CHANGING
        tree                 TYPE tt_tree.
    METHODS get_superclasses
      IMPORTING
        original_object_name TYPE eu_lname
      RETURNING
        VALUE(superclasses)  TYPE seo_relkeys.
    METHODS add_class_additional_info
      IMPORTING
        object_name TYPE eu_lname
        object_type TYPE seu_obj
      CHANGING
        tree        TYPE tt_tree.
    METHODS add_visibility
      IMPORTING
        original_object_name TYPE eu_lname
        original_object_type TYPE seu_obj
      CHANGING
        tree                 TYPE tt_tree.
    METHODS set_visibility
      CHANGING
        node TYPE snodetext.
    METHODS set_object_type
      CHANGING
        node TYPE snodetext.
    METHODS add_local_classes_attributes
      CHANGING
        tree TYPE tt_tree.
    METHODS add_redefinitions
      IMPORTING
        original_object_name TYPE eu_lname
        original_object_type TYPE seu_obj
      CHANGING
        tree                 TYPE tt_tree.
    METHODS parse_parameters
      IMPORTING
        parameters TYPE tihttpnvp.
    METHODS get_first_level_subclasses
      IMPORTING
        class_name        TYPE eu_lname
      RETURNING
        VALUE(subclasses) TYPE seo_relkeys.
    METHODS get_first_lvl_of_redefinitions
      IMPORTING
        class_name           TYPE eu_lname
      RETURNING
        VALUE(redefinitions) TYPE seor_redefinitions_r.
    METHODS adapt_attiributes
      IMPORTING
        att                  TYPE t_attribute
        original_object_name TYPE eu_lname
        node                 TYPE REF TO snodetext.
    METHODS build_interfaces_range
      IMPORTING
        tree                    TYPE zcl_adtco_tree_creator=>tt_tree
      RETURNING
        VALUE(interfaces_range) TYPE ty_class_names.
ENDCLASS.



CLASS zcl_adtco_tree_creator IMPLEMENTATION.


  METHOD add_sublcasses.
    CHECK relations IS NOT INITIAL.

    ASSIGN tree[ KEY type COMPONENTS type = 'COU' ] TO FIELD-SYMBOL(<parent>).
    IF sy-subrc EQ 0.
      DATA(counter) = get_counter( tree ).
      DATA(subclasses) = get_subclasses( original_object_name ).
      get_subclasses_description( original_object_name ).
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
    parse_parameters( parameters ).
    DATA(tree_object_type) = get_object_type( object_type ).
    actualize_program_tree( object_type      = object_type
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
    add_class_additional_info( EXPORTING object_name = object_name
                                         object_type = object_type
                               CHANGING  tree        = tree ).
  ENDMETHOD.

  METHOD add_class_additional_info.

    DATA: line LIKE LINE OF tree.
    MODIFY TABLE tree FROM line TRANSPORTING kind3 kind4 kind5 kind6 kind7 kind8 kind9.

    add_superclasses( EXPORTING original_object_name = object_name
                                original_object_type = object_type
                      CHANGING  tree                 = tree ).

    add_sublcasses( EXPORTING original_object_name = object_name
                              original_object_type = object_type
                    CHANGING  tree                 = tree ).

    add_visibility( EXPORTING original_object_name = object_name
                              original_object_type = object_type
                    CHANGING  tree                 = tree ).

    add_redefinitions( EXPORTING original_object_name = object_name
                                 original_object_type = object_type
                       CHANGING  tree                 = tree ).
    add_local_classes_attributes( CHANGING tree = tree ).

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
    ASSIGN subclasses_descriptions[ clsname = class_name ] TO FIELD-SYMBOL(<sc>).
    IF sy-subrc EQ 0.
      description = <sc>-descript.
      RETURN.
    ENDIF.
    SELECT SINGLE descript  INTO @description
    FROM seoclasstx
    WHERE clsname EQ @class_name
    AND langu EQ @sy-langu.
  ENDMETHOD.

  METHOD get_subclasses_description.
    DATA(subclasses) = get_subclasses( original_object_name ).
    CHECK subclasses IS NOT INITIAL.
    SELECT clsname, descript  INTO CORRESPONDING FIELDS OF TABLE @subclasses_descriptions
    FROM seoclasstx
    FOR ALL ENTRIES IN @subclasses
    WHERE clsname EQ @subclasses-clsname
    AND langu EQ @sy-langu.
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
    IF call_parameters-load_all_levels_of_subclasses EQ abap_false.
      subclasses = get_first_level_subclasses( object_name ).
    ELSE.
      subclasses = VALUE #( FOR <sc> IN relations->subclasses ( clsname = <sc>-clsname ) ).
    ENDIF.
    SORT subclasses BY clsname.
    DELETE ADJACENT DUPLICATES FROM subclasses COMPARING clsname.
  ENDMETHOD.

  METHOD add_superclasses.
    CHECK original_object_type EQ class_type.
    DATA(superclasses) = get_superclasses( CONV #( original_object_name ) ).
    ASSIGN tree[ KEY type COMPONENTS type = 'COS' ] TO FIELD-SYMBOL(<parent>).
    IF sy-subrc EQ 0.
      DATA(counter) = get_counter( tree ).
      LOOP AT superclasses ASSIGNING FIELD-SYMBOL(<superclass>).
        ASSIGN tree[ KEY method COMPONENTS type = 'OOC' text1 = <superclass>-clsname ] TO <parent>.
        IF sy-subrc EQ 0.
          IF <parent>-child IS INITIAL.
            <parent>-child = counter.
          ENDIF.
          APPEND VALUE #( text1 = <superclass>-refclsname parent = <parent>-id id = counter type = 'OOC' text2 = get_class_description( <superclass>-refclsname )  ) TO tree.
          ADD 1 TO counter.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_superclasses.
    relations = NEW cl_oo_class_relations(
      clsname         = CONV #( original_object_name )
      w_superclasses  = abap_true
      w_subclasses    = COND #( WHEN call_parameters-load_all_levels_of_subclasses EQ abap_true
                                 OR call_parameters-load_all_levels_of_redef EQ abap_true THEN abap_true
                                ELSE abap_false )
      w_references    = abap_false
      w_redefinitions = COND #( WHEN call_parameters-load_redefinitions_of_method EQ abap_true
                                 AND call_parameters-load_all_levels_of_redef EQ abap_true THEN abap_true
                                 ELSE abap_false )
      w_eventhandler  = abap_false
      w_implementings = abap_false ).
    LOOP AT relations->superclasses ASSIGNING FIELD-SYMBOL(<superclass>)
                                    WHERE clsname NE original_object_name.
      IF NOT line_exists( superclasses[ clsname = <superclass>-clsname refclsname = <superclass>-refclsname ] ).
        APPEND VALUE #( clsname = <superclass>-clsname refclsname = <superclass>-refclsname ) TO superclasses.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_visibility.
    CHECK original_object_type EQ class_type OR
          original_object_type EQ interface_type.

    data: attributes type sorted table of t_attribute with NON-UNIQUE key clsname cmpname version.
    DATA(class_names) = build_class_range( original_object_name ).
    IF original_object_type EQ class_type.
      DATA(interfaces_mames) = build_interfaces_range( tree ).
      APPEND LINES OF interfaces_mames TO class_names.
    ENDIF.


    SELECT seocompo~clsname, seocompo~cmpname, version, attrdonly, mtdabstrct, mtdfinal, mtddecltyp, attdecltyp, exposure,typtype, refcmpname,
          cmptype, mtdtype
      INTO CORRESPONDING FIELDS OF TABLE @attributes
      FROM seocompodf
      INNER JOIN seocompo
      ON seocompo~clsname EQ seocompodf~clsname
      AND seocompo~cmpname EQ  seocompodf~cmpname
      WHERE seocompo~clsname IN @class_names.

    DELETE ADJACENT DUPLICATES FROM attributes COMPARING clsname cmpname.

    LOOP AT attributes ASSIGNING FIELD-SYMBOL(<att>).
      IF NOT <att>-clsname IN interfaces_mames OR interfaces_mames IS INITIAL.
        LOOP AT tree ASSIGNING FIELD-SYMBOL(<tree>) USING KEY method WHERE text1 = <att>-cmpname
                                                      AND type NP 'OOL*'
                                                      AND text8 NP '*~*'.
          adapt_attiributes(
            EXPORTING
              att                  = <att>
              original_object_name = original_object_name
              node                 = REF #( <tree> ) ).
        ENDLOOP.
      ELSE.
        DATA(name_pattern) = |{ <att>-clsname }~{ <att>-cmpname }|.
        LOOP AT tree ASSIGNING <tree> USING KEY method WHERE text1 = <att>-cmpname
                                                      AND type NP 'OOL*'
                                                      AND text8 EQ name_pattern.
          adapt_attiributes(
            EXPORTING
              att                  = <att>
              original_object_name = original_object_name
              node                 = REF #( <tree> ) ).
        ENDLOOP.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.

  METHOD adapt_attiributes.

    IF att-cmptype EQ 3 AND att-clsname NE original_object_name AND node->text8 NP '*~*'.
      node->text8 = att-clsname.
    ENDIF.
    IF att-attrdonly EQ abap_true.
      node->kind7 = abap_true.
    ENDIF.
    IF att-mtdfinal EQ abap_true.
      node->kind8 = abap_true.
    ENDIF.
    node->kind9 = COND #( WHEN att-attdecltyp IS NOT INITIAL THEN att-attdecltyp
                           WHEN att-mtddecltyp IS NOT INITIAL THEN att-mtddecltyp
                           ELSE 0  ).

    IF att-mtdabstrct EQ abap_true.
      node->kind6 = abap_true.
    ENDIF.
    node->kind5 = att-exposure.
    node->kind4 = att-cmptype.
    IF node->kind4 EQ 2.
      CLEAR node->kind4.
    ENDIF.
    IF att-mtdtype EQ 1.
      node->kind4 = 2.
    ENDIF.

  ENDMETHOD.



  METHOD add_local_classes_attributes.

    FIELD-SYMBOLS <tree> TYPE snodetext.

    LOOP AT tree ASSIGNING <tree>  WHERE type CP 'OOL*'
                                    OR type CP 'OON*'
                                    OR  type EQ 'OOL'
                                    OR  type EQ 'OPL'.

      set_object_type( CHANGING node = <tree> ).
      set_visibility( CHANGING node = <tree> ).

      IF <tree>-text6+7(1) EQ 'X'."read only
        <tree>-kind7 = abap_true.
      ENDIF.
      IF <tree>-text6(1) EQ 'X'. "final
        <tree>-kind8 = abap_true.
      ENDIF.
      IF <tree>-text6+6(1) EQ 'X'."static
        <tree>-kind9 = 1.
      ENDIF.
      IF <tree>-text6+1(1) EQ 'X'. "abstract
        <tree>-kind6 = abap_true.
      ENDIF.
      IF <tree>-text6+2(1) EQ 'X'. "for testing
        <tree>-kind3 = abap_true.
      ENDIF.
      IF <tree>-text2 EQ 'constant'.
        <tree>-kind9 = 2.
        <tree>-kind4 = 0.
      ELSEIF <tree>-text2 EQ 'event handler'.
        <tree>-kind4 = 2.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.



  METHOD set_object_type.

    node-kind4 = SWITCH #( node-type WHEN 'OOLT' OR 'OONT'  THEN 3
                                         WHEN 'OOLA' OR 'OONA' THEN 0
                                         WHEN 'OOLD' OR 'OOND' THEN 1
                                         WHEN 'OOLI' OR 'OONI' THEN 1
                                         WHEN 'OOLE' OR 'OONE' THEN 2
                                         ELSE space ).

  ENDMETHOD.



  METHOD set_visibility.

    node-kind5 = SWITCH #( node-name WHEN '@5B@' THEN 2
                                         WHEN '@5C@' THEN 0
                                         WHEN '@5D@' THEN 1 ).

  ENDMETHOD.



  METHOD build_class_range.
    IF  relations IS NOT INITIAL.
      LOOP AT relations->superclasses ASSIGNING FIELD-SYMBOL(<superclass>).
        IF sy-tabix EQ 1.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <superclass>-clsname ) TO class_names.
        ENDIF.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <superclass>-refclsname ) TO class_names.
      ENDLOOP.
      IF sy-subrc NE 0.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = original_object_name ) TO class_names.
      ENDIF.
    ELSE.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = original_object_name ) TO class_names.
    ENDIF.
  ENDMETHOD.




  METHOD add_redefinitions.
    DATA: redefinitions TYPE seor_redefinitions_r.
    CHECK relations IS NOT INITIAL AND
          call_parameters-load_redefinitions_of_method EQ abap_true.

    IF call_parameters-load_all_levels_of_redef EQ abap_false.
      redefinitions = get_first_lvl_of_redefinitions( original_object_name ).
    ELSE.
      redefinitions = relations->redefinitions.
    ENDIF.
    DATA(counter) = get_counter( tree ).
    LOOP AT redefinitions ASSIGNING FIELD-SYMBOL(<red>).
      ASSIGN tree[ KEY method COMPONENTS text1 = <red>-mtdname type = 'OOM'  ] TO FIELD-SYMBOL(<parent>).
      IF sy-subrc EQ 0.
        IF <parent>-child IS INITIAL.
          <parent>-child = counter.
        ENDIF.
        APPEND VALUE #( text1 = <red>-clsname parent = <parent>-id id = counter type = zcl_adtco_uri_mapper=>node_types-method_redefintion_class text2 = get_class_description( <red>-clsname ) text8 = <red>-mtdname ) TO tree.
        ADD 1 TO counter.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD parse_parameters.
    CONSTANTS fetchredefinitionsformethods TYPE string VALUE 'FetchRedefinitionsForMethods' ##NO_TEXT.
    CONSTANTS loadalllevelsofsubclasses TYPE string VALUE 'LoadAllLevelsOfSubclasses' ##NO_TEXT.
    CONSTANTS loadalllevelsofredefinitions TYPE string VALUE 'LoadAllLevelsOfRedefinitions' ##NO_TEXT.
    LOOP AT parameters ASSIGNING FIELD-SYMBOL(<par>).
      CASE <par>-name.
        WHEN fetchredefinitionsformethods.
          call_parameters-load_redefinitions_of_method = <par>-value.
        WHEN loadalllevelsofsubclasses.
          call_parameters-load_all_levels_of_subclasses = <par>-value.
        WHEN loadalllevelsofredefinitions.
          call_parameters-load_all_levels_of_redef = <par>-value.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_first_level_subclasses.
    SELECT clsname , refclsname FROM vseoextend
         INTO CORRESPONDING FIELDS OF TABLE @subclasses
         WHERE refclsname = @class_name
         AND ( version = '0' OR version = '1' ).
  ENDMETHOD.


  METHOD get_first_lvl_of_redefinitions.
    SELECT * FROM seoredef INTO CORRESPONDING FIELDS OF TABLE @redefinitions
    WHERE refclsname = @class_name.
    SORT redefinitions BY clsname mtdname.
    DELETE ADJACENT DUPLICATES FROM redefinitions COMPARING clsname mtdname.
  ENDMETHOD.


  METHOD build_interfaces_range.
    interfaces_range = VALUE #( FOR <node> IN tree USING KEY type WHERE ( type EQ 'OOI'   ) ( sign = 'I' option = 'EQ' low = <node>-text1 ) ).
  ENDMETHOD.

ENDCLASS.
