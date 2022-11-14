CLASS zcl_adtco_uri_mapper DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF node_types,
                 lcl_interface_type        TYPE string VALUE 'OONT' ##NO_TEXT,
                 lcl_interface_method_def  TYPE string VALUE 'OOND' ##NO_TEXT,
                 lcl_interface_attribute   TYPE string VALUE 'OONA' ##NO_TEXT,
                 lcl_interface_interface   TYPE string VALUE 'OONN' ##NO_TEXT,
                 lcl_method_definition     TYPE string VALUE 'OOLD' ##NO_TEXT,
                 lcl_method_implementation TYPE string VALUE 'OOLI' ##NO_TEXT,
                 lcl_attribute             TYPE string VALUE 'OOLA' ##NO_TEXT,
                 lcl_type                  TYPE string VALUE 'OOLT' ##NO_TEXT,
                 lcl_interface             TYPE string VALUE 'OOLN' ##NO_TEXT,
                 method_redefintion_class  TYPE string VALUE 'ROM' ##NO_TEXT,
               END OF node_types.
    CLASS-METHODS: get_instance RETURNING VALUE(uri_mapper) TYPE REF TO zcl_adtco_uri_mapper.
    METHODS: get_uri_for_tree_node IMPORTING VALUE(node)        TYPE snodetext
                                             VALUE(object_name) TYPE eu_lname
                                             VALUE(object_type) TYPE seu_obj
                                   RETURNING VALUE(uri)         TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF prefix,
                 reps          TYPE string VALUE 'REPS' ##NO_TEXT,
                 type          TYPE string VALUE 'TYPE' ##NO_TEXT,
                 fugr_pattern  TYPE string VALUE 'FUGR/*' ##NO_TEXT,
                 class_pattern TYPE string VALUE 'CLAS*' ##NO_TEXT,
                 adt_fg        TYPE string VALUE `/sap/bc/adt/functions/groups/` ##NO_TEXT,
                 adt_program   TYPE string VALUE `/sap/bc/adt/programs/` ##NO_TEXT,
               END OF prefix,
               BEGIN OF object_types,
                 fm              TYPE string VALUE 'FUGR/FF',
                 fg              TYPE string VALUE 'FUGR/F',
                 fg_include      TYPE string VALUE 'FUGR/I',
                 interface       TYPE string VALUE 'INTF',
                 report_include  TYPE string VALUE 'REPS',
                 program_include TYPE string VALUE 'PROG/I',
               END OF object_types,
               BEGIN OF context,
                 fg                           TYPE string VALUE `/source/main?context=%2fsap%2fbc%2fadt%2ffunctions%2fgroups%2f` ##NO_TEXT,
                 program_lcl_method           TYPE string VALUE `/source/main#type=PROG%2FPLM;name=` ##NO_TEXT,
                 fg_lcl_method                TYPE string VALUE `#type=PROG%2FPLM;name=` ##NO_TEXT,
                 includes                     TYPE string VALUE 'includes/' ##NO_TEXT,
                 programs                     TYPE string VALUE 'programs/' ##NO_TEXT,
                 program_lcl_interface_types  TYPE string VALUE `/source/main#type=PROG%2FPNY;name=` ##NO_TEXT,
                 program_lcl_attribute        TYPE string VALUE `/source/main#type=PROG%2FPLA;name=` ##NO_TEXT,
                 fg_lcl_attribute             TYPE string VALUE `#type=PROG%2FPLA;name=` ##NO_TEXT,
                 program_lcl_type             TYPE string VALUE `/source/main#type=PROG%2FPLY;name=` ##NO_TEXT,
                 program_lcl_interface        TYPE string VALUE `/source/main#type=PROG%2FPN;name=` ##NO_TEXT,
                 program_lcl_interface_method TYPE string VALUE `/source/main#type=PROG%2FPNM;name=` ##NO_TEXT,
                 program_lcl_interface_attrib TYPE string VALUE `/source/main#type=PROG%2FPNA;name=` ##NO_TEXT,
                 program_lcl_interface_interf TYPE string VALUE `/source/main#type=PROG%2FPNN;name=` ##NO_TEXT,
               END OF context.


    CLASS-DATA: uri_mapper TYPE REF TO zcl_adtco_uri_mapper.
    DATA do_escaping TYPE abap_bool.
    METHODS build_object_name
      IMPORTING
        node               TYPE REF TO snodetext
      RETURNING
        VALUE(object_name) TYPE eu_lname.
    METHODS build_enclosed_object
      IMPORTING
        object_name            TYPE eu_lname
        object_type            TYPE seu_obj
        node                   TYPE REF TO snodetext
      RETURNING
        VALUE(enclosed_object) TYPE char70.
    METHODS update_original_class_name
      IMPORTING
        class_name TYPE eu_lname
      CHANGING
        uri        TYPE string.
    METHODS get_origin_class_name
      IMPORTING
                node                     TYPE snodetext
                class_name               TYPE eu_lname
                objtype                  TYPE seu_objtyp
      RETURNING VALUE(origin_class_name) TYPE eu_lname.
    METHODS build_node_type
      IMPORTING
        node             TYPE snodetext
      RETURNING
        VALUE(node_type) TYPE seu_type .
    METHODS get_object_name
      IMPORTING
        original_object_name TYPE eu_lname
        original_object_type TYPE seu_obj
      RETURNING
        VALUE(object_name)   TYPE eu_lname.
    METHODS get_uri_directly
      IMPORTING
        node                        TYPE snodetext
        VALUE(original_object_name) TYPE eu_lname
        original_object_type        TYPE seu_obj
      RETURNING
        VALUE(uri)                  TYPE string.
    METHODS build_internal_name
      IMPORTING
        node                 TYPE snodetext
      RETURNING
        VALUE(internal_name) TYPE string .
    METHODS get_program_or_include
      IMPORTING
        node                 TYPE snodetext
        original_object_name TYPE eu_lname
      RETURNING
        VALUE(type)          TYPE string.
    METHODS get_fg_name_from_object
      IMPORTING
        original_object_name TYPE eu_lname
        original_object_type TYPE seu_obj
      RETURNING
        VALUE(object_name)   TYPE eu_lname.
    METHODS update_object_name_for_fg
      IMPORTING
        original_object_type TYPE seu_obj
        original_object_name TYPE eu_lname
      RETURNING
        VALUE(object_name)   TYPE eu_lname.
    METHODS add_uri_prefix
      IMPORTING
        current_uri          TYPE string
        original_object_type TYPE seu_obj
        original_object_name TYPE eu_lname
      RETURNING
        VALUE(uri)           TYPE string.
    METHODS get_context_for_node_type
      IMPORTING
        node_type           TYPE snodetext-type
      RETURNING
        VALUE(node_context) TYPE string.
    METHODS build_program_uri
      IMPORTING
        node                 TYPE snodetext
        original_object_name TYPE eu_lname
      RETURNING
        VALUE(r_result)      TYPE string.
    METHODS build_fg_uri
      IMPORTING
        node                 TYPE snodetext
        original_object_name TYPE eu_lname
      RETURNING
        VALUE(r_result)      TYPE string.
    METHODS get_context_for_fg_node_type
      IMPORTING
        node_type           TYPE snodetext-type
      RETURNING
        VALUE(node_context) TYPE string.
    METHODS get_fg_main_program_name
      IMPORTING
        function_group      TYPE eu_lname
      RETURNING
        VALUE(main_program) TYPE char70.
    METHODS adapt_uri_for_subclasses
      IMPORTING
        object_type        TYPE seu_obj
        VALUE(object_name) TYPE eu_lname
        node               TYPE snodetext
        objtype            TYPE seu_objtyp
      CHANGING
        uri                TYPE string.
    METHODS escape_string
      IMPORTING input                 TYPE any
      RETURNING VALUE(escaped_string) TYPE string.
    METHODS get_uri_for_semantic_object
      IMPORTING
        object_type TYPE seu_obj
        object_name TYPE eu_lname
        node        TYPE snodetext
      RETURNING
        VALUE(uri)  TYPE string
      RAISING
        cx_adt_uri_mapping.

ENDCLASS.



CLASS zcl_adtco_uri_mapper IMPLEMENTATION.
  METHOD get_instance.
    IF zcl_adtco_uri_mapper=>uri_mapper IS NOT BOUND.
      zcl_adtco_uri_mapper=>uri_mapper = NEW #( ).
    ENDIF.
    uri_mapper = zcl_adtco_uri_mapper=>uri_mapper.
  ENDMETHOD.

  METHOD build_node_type.
    node_type = node-type.
    IF node-type+1(3) EQ swbm_c_type_cls_type AND node-text8 CP '*~*'.
      node_type+1(3) = swbm_c_type_intf_type.
    ENDIF.
    IF node-type+1(3) EQ swbm_c_type_cls_attribute AND node-text8 IS NOT INITIAL.
      node_type+1(3) = swbm_c_type_intf_attribute.
    ENDIF.
    IF node-type+1(3) EQ swbm_c_type_cls_lintf_intf AND node-text8 IS NOT INITIAL.
      node_type+1(3) = swbm_c_type_prg_intf_def.
    ENDIF.
    IF node-type+1(3) EQ swbm_c_type_class AND node-text8 IS NOT INITIAL.
      node_type+1(3) = swbm_c_type_cls_mtd_impl.
    ENDIF.
  ENDMETHOD.

  METHOD get_uri_for_tree_node.
    CHECK node-type(1) NE 'C'.
    node-type = build_node_type( node ).

    DATA(wb) = cl_wb_object=>create_from_toolaccess_key(
             p_object_type           = CONV #( node-type+1(3) )
             p_object_name           = build_object_name( REF #( node ) )
             p_enclosing_object      = build_enclosed_object(
                                                    object_name = object_name
                                                    object_type = object_type
                                                    node       = REF #( node ) )
             ).
    IF sy-subrc EQ 0.
      wb->get_request_key(
        IMPORTING
          p_object_type     = DATA(objtype)
          p_object_name     = DATA(objkey)
        EXCEPTIONS
          key_not_available = 1
          OTHERS            = 2
      ).
      IF sy-subrc EQ 0.

        cl_wb_request=>create_from_object_ref(
          EXPORTING
            p_wb_object       = wb
          RECEIVING
            p_wb_request      = DATA(wb_request)
          EXCEPTIONS
            illegal_operation = 1
            cancelled         = 2
            OTHERS            = 3
        ).
        IF sy-subrc EQ 0.
          TRY.
              DATA(adt_reference) = cl_adt_tools_core_factory=>get_instance( )->get_uri_mapper( )->map_wb_request_to_objref( wb_request ).
              uri = adt_reference->ref_data-uri.
              adapt_uri_for_subclasses(
                EXPORTING
                  object_type = object_type
                  object_name = object_name
                  node        = node
                  objtype     = objtype
                CHANGING
                  uri         = uri ).
            CATCH cx_adt_uri_mapping.
              TRY.
                  uri = get_uri_for_semantic_object( object_type = object_type
                                                     object_name = object_name
                                                     node        = node ).
                CATCH cx_root.

                  uri = get_uri_directly( node                 = node
                                          original_object_name = object_name
                                          original_object_type = object_type ).
                  IF uri IS NOT INITIAL.
                    RETURN.
                  ENDIF.
              ENDTRY.
          ENDTRY.


        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_uri_for_semantic_object.

    DATA wb_request TYPE REF TO cl_wb_request.

    cl_wb_request=>create_from_encl_name(
      EXPORTING
        p_r3tr_type         = CONV #( object_type )
        p_object_type           = CONV #( node-type+1(3) )
        p_object_name           = build_object_name( REF #( node ) )
        p_encl_object_name      = build_enclosed_object(
                                            object_name = object_name
                                            object_type = object_type
                                            node       = REF #( node ) )
        p_operation         = swbm_c_op_display
      RECEIVING
        p_wb_request        = wb_request
    EXCEPTIONS
      OTHERS              = 0 ).
    DATA(adtcore) = cl_adt_tools_core_factory=>get_instance( ).
    DATA: semantic_builder TYPE REF TO object.
    DATA: adt_ref TYPE REF TO cl_adt_object_reference.
    CALL METHOD adtcore->('GET_SEMANTIC_URI_BUILDER')
      RECEIVING
        result = semantic_builder.

    CALL METHOD semantic_builder->('IF_ADT_SEMANTIC_URI_BUILDER~MAP_WB_REQUEST_TO_OBJREF')
      EXPORTING
        i_wb_request    = wb_request
        i_include_w_pos = CONV syrepid( node-text9+4 )
      RECEIVING
        result          = adt_ref.
    uri = adt_ref->ref_data-uri.
    do_escaping = abap_true.
  ENDMETHOD.







  METHOD adapt_uri_for_subclasses.
    CHECK NOT ( node-type = node_types-method_redefintion_class ).
    IF object_type CP prefix-class_pattern .
      object_name = get_origin_class_name( node       = node
                                           class_name = object_name
                                           objtype    = objtype ).
      update_original_class_name( EXPORTING class_name = object_name
                                  CHANGING  uri        = uri ).
    ENDIF.

  ENDMETHOD.



  METHOD get_origin_class_name.
    origin_class_name = class_name.
    CASE objtype.
      WHEN swbm_c_type_cls_attribute OR swbm_c_type_cls_mtd_def OR swbm_c_type_cls_evt OR swbm_c_type_cls_type.
        DATA componentkey TYPE seocmpkey.
        CALL FUNCTION 'SEO_COMPONENT_BY_INHERITANCE'
          EXPORTING
            cpdkey       = VALUE seocpdkey( clsname = class_name cpdname = build_object_name( REF #( node ) ) )
          IMPORTING
            cmpkey       = componentkey
          EXCEPTIONS
            not_existing = 1
            model_only   = 2
            OTHERS       = 3.
        IF ( sy-subrc = 0 ).
          origin_class_name = componentkey-clsname.
        ENDIF.
      WHEN swbm_c_type_cls_mtd_impl.
        DATA methodkey TYPE seocpdkey.
        methodkey = VALUE #( clsname = class_name cpdname = build_object_name( REF #( node ) )  ).
        TRANSLATE methodkey TO UPPER CASE.

        CALL FUNCTION 'SEO_METHOD_GET_YOUNGEST'
          EXPORTING
            cpdkey       = methodkey
          IMPORTING
            youngest     = methodkey
          EXCEPTIONS
            not_existing = 1
            OTHERS       = 2.
        IF ( sy-subrc = 0 ).
          origin_class_name = methodkey-clsname.
        ENDIF.
    ENDCASE.

  ENDMETHOD.



  METHOD update_original_class_name.
    DATA(escaped_class_name) = escape_string( class_name ).
    REPLACE FIRST OCCURRENCE OF REGEX '/sap/bc/adt/oo/classes/(.*)/source/'
                IN uri WITH |/sap/bc/adt/oo/classes/{ to_lower( escaped_class_name ) }/source/|.

  ENDMETHOD.

  METHOD build_object_name.
    CASE node->type+1(3).
      WHEN swbm_c_type_cls_local_type OR
           swbm_c_type_cls_local_intf OR
           swbm_c_type_cls_lintf_attr OR
           swbm_c_type_cls_lintf_event OR
           swbm_c_type_cls_lintf_type OR
           swbm_c_type_cls_lintf_intf .
        object_name = |{ node->text9+4(36) }                                   { node->text1 }|.
      WHEN swbm_c_type_cls_mtd_impl." OR swbm_c_type_intf_type.
        IF node->text8 IS NOT INITIAL.
          object_name = node->text8.
        ELSE.
          object_name = node->text1.
        ENDIF.
      WHEN swbm_c_type_intf_attribute OR swbm_c_type_intf_type.
        IF node->text8 IS NOT INITIAL.
          SPLIT node->text8 AT '~' INTO DATA(object) object_name.
          IF object_name IS INITIAL.
            IF strlen( node->text9 ) GE 4 AND node->text9(4) EQ object_types-interface.
              object_name = node->text9.
              SHIFT object_name BY 4 PLACES LEFT.
            ENDIF.
          ENDIF.
        ELSE.
          object_name = node->text1.
        ENDIF.
      WHEN OTHERS.
        object_name = node->text1 .
    ENDCASE.
    IF node->type EQ node_types-method_redefintion_class.
      object_name = node->text8.
    ENDIF.
  ENDMETHOD.


  METHOD build_enclosed_object.

    CASE node->type+1(3).
      WHEN   swbm_c_type_cls_loc_meth_def OR
             swbm_c_type_cls_loc_meth_impl  OR
             swbm_c_type_cls_local_attr     OR
             swbm_c_type_cls_local_event    OR
             swbm_c_type_cls_local_type2    OR
             swbm_c_type_cls_local_intf2 OR
             swbm_c_type_cls_lintf_attr OR
             swbm_c_type_cls_lintf_event OR
             swbm_c_type_cls_lintf_type OR
             swbm_c_type_cls_lintf_intf OR
             swbm_c_type_cls_lintf_meth.
        enclosed_object = node->text8.
        IF object_type EQ object_types-fg.
          enclosed_object(40) = get_fg_main_program_name( object_name ).
        ENDIF.
      WHEN swbm_c_type_intf_type OR
           swbm_c_type_intf_attribute.
        IF node->text8 IS NOT INITIAL.
          SPLIT node->text8 AT '~' INTO enclosed_object DATA(tmp_object_name).
        ELSE.
          enclosed_object = object_name.
        ENDIF.
      WHEN swbm_c_type_cls_type.
        IF node->text8 IS NOT INITIAL.
          enclosed_object = node->text8.
        ELSE.
          enclosed_object = get_object_name(
            original_object_name = object_name
            original_object_type = object_type ).
        ENDIF.
      WHEN OTHERS.
        enclosed_object = get_object_name(
          original_object_name = object_name
          original_object_type = object_type ).
    ENDCASE.

    IF node->type EQ node_types-method_redefintion_class.
      enclosed_object = node->text1.
    ENDIF.

  ENDMETHOD.

  METHOD get_fg_main_program_name.

    IF function_group(1) EQ '/'.
      DATA(regex) = NEW cl_abap_regex(   pattern       =  '\/.*\/(.*)' ).
      DATA(matcher) = regex->create_matcher( text = function_group ).
      IF matcher->match( ).
        DATA(offset) = matcher->get_offset( index = 1 ).
        main_program(40) = |{ function_group+0(offset) }SAPL{ matcher->get_submatch( index = 1 ) }|.
      ENDIF.
    ELSE.
      main_program(40) = |SAPL{ function_group }|.
    ENDIF.

  ENDMETHOD.



  METHOD get_object_name.
    CASE original_object_type.
      WHEN object_types-fm.
        SELECT SINGLE pname FROM tfdir
        INTO object_name
        WHERE funcname = original_object_name.
      WHEN object_types-fg.
        object_name = get_fg_main_program_name( original_object_name ).
      WHEN object_types-fg_include.
        DATA string_name TYPE string.
        CALL FUNCTION 'Z_ADTCO_GET_INC_MASTER_PROGRAM'
          EXPORTING
            include = CONV string( original_object_name )
          IMPORTING
            master  = string_name.
        object_name = get_fg_main_program_name( CONV #( string_name ) ).
      WHEN object_types-report_include OR object_types-program_include.
        CALL FUNCTION 'Z_ADTCO_GET_INC_MASTER_PROGRAM'
          EXPORTING
            include = CONV string( original_object_name )
          IMPORTING
            master  = string_name.
        object_name = COND #( WHEN string_name IS NOT INITIAL THEN string_name
                              ELSE original_object_name ).
      WHEN OTHERS.
        object_name = original_object_name.
    ENDCASE.
  ENDMETHOD.

  METHOD get_fg_name_from_object.
    CASE original_object_type.
      WHEN object_types-fg_include.
        object_name = original_object_name.
        IF object_name(1) EQ '/'.
          REPLACE FIRST OCCURRENCE OF REGEX '\/L' IN object_name WITH '/'.
        ELSE.
          SHIFT object_name BY 1 PLACES LEFT.
        ENDIF.
        DATA(lenght) = strlen( object_name ) - 3.
        object_name = object_name(lenght).
      WHEN object_types-fm.
        SELECT SINGLE pname FROM tfdir
        INTO object_name
        WHERE funcname = original_object_name.
        REPLACE FIRST OCCURRENCE OF 'SAPL' IN object_name WITH ''.
    ENDCASE.
  ENDMETHOD.



  METHOD get_uri_directly.

    CHECK node-text9(4) EQ prefix-reps OR
          node-text9(4) EQ prefix-type.


    original_object_name = update_object_name_for_fg( original_object_type = original_object_type
                                                      original_object_name = original_object_name ).


    uri = SWITCH #( node-type WHEN node_types-lcl_interface_type OR
                                   node_types-lcl_interface_method_def OR
                                   node_types-lcl_interface_attribute OR
                                   node_types-lcl_interface_interface OR
                                   node_types-lcl_type OR
                                   node_types-lcl_interface
                                                      THEN build_program_uri( node = node original_object_name = original_object_name )
                              WHEN node_types-lcl_method_definition OR
                                   node_types-lcl_method_implementation OR
                                   node_types-lcl_attribute
                                                    THEN COND #( WHEN original_object_type CP prefix-fugr_pattern
                                                                      THEN build_fg_uri( node = node original_object_name = original_object_name )
                                                                 ELSE build_program_uri( node = node original_object_name = original_object_name ) )
                              WHEN 'OPG' THEN |source/main#type=FUGR/PG;name={ node-text1 }|

                              ELSE space ).

    IF uri IS NOT INITIAL.
      uri = add_uri_prefix(
        current_uri          = uri
        original_object_type = original_object_type
        original_object_name = original_object_name ).
    ENDIF.
  ENDMETHOD.

  METHOD build_fg_uri.
    r_result = |{ context-includes }{
     escape_string( node-text8(40) ) }{ context-fg }{
      escape_string( original_object_name ) }{ get_context_for_fg_node_type( node-type ) }{ build_internal_name( node ) }|.
  ENDMETHOD.

  METHOD build_program_uri.
    r_result = |{ get_program_or_include( node = node original_object_name = original_object_name )
                                                            }{ escape_string( node-text8(40)  ) }{
           get_context_for_node_type( node-type ) }{ build_internal_name( node )  }|.
  ENDMETHOD.

  METHOD add_uri_prefix.

    IF original_object_type  CP prefix-fugr_pattern.
      uri = |{ prefix-adt_fg }{  escape_string( original_object_name ) }/{ current_uri }|.
    ELSE.
      uri = |{ prefix-adt_program }{ current_uri }|.
    ENDIF.

  ENDMETHOD.



  METHOD update_object_name_for_fg.

    IF original_object_type CP prefix-fugr_pattern.
      DATA(fg_name) = get_fg_name_from_object(
        original_object_name = original_object_name
        original_object_type = original_object_type
      ).
      IF fg_name IS NOT INITIAL.
        object_name = fg_name.
      ELSE.
        object_name = original_object_name.
      ENDIF.
    ELSE.
      object_name = original_object_name.
    ENDIF.

  ENDMETHOD.



  METHOD build_internal_name.
    internal_name = escape( val = |{ node-text8+40(30) WIDTH = 30  }{ node-text1 }| format = cl_abap_format=>e_uri ).
  ENDMETHOD.


  METHOD get_program_or_include.
    DATA: program_name TYPE trdir-name,
          program_type TYPE trdir-subc.
    program_name = node-text8.
    SELECT SINGLE subc INTO @program_type FROM trdir WHERE name EQ @program_name.
    IF program_type EQ 'I'.
      type = context-includes.
    ELSE.
      type = context-programs.
    ENDIF.
  ENDMETHOD.


  METHOD get_context_for_node_type.
    node_context = SWITCH #( node_type WHEN node_types-lcl_interface_type THEN context-program_lcl_interface_types
                                       WHEN node_types-lcl_interface_method_def THEN context-program_lcl_interface_method
                                       WHEN node_types-lcl_interface_attribute THEN context-program_lcl_interface_attrib
                                       WHEN node_types-lcl_interface_interface THEN context-program_lcl_interface_interf
                                       WHEN node_types-lcl_type THEN context-program_lcl_type
                                       WHEN node_types-lcl_interface THEN context-program_lcl_interface
                                       WHEN node_types-lcl_attribute THEN context-program_lcl_attribute
                                       WHEN node_types-lcl_method_implementation THEN context-program_lcl_method ).
  ENDMETHOD.


  METHOD get_context_for_fg_node_type.
    node_context = SWITCH #( node_type WHEN node_types-lcl_attribute THEN context-fg_lcl_attribute
                                       WHEN node_types-lcl_method_implementation THEN context-fg_lcl_method
                                       WHEN node_types-lcl_method_definition THEN context-fg_lcl_method  ).
  ENDMETHOD.

  METHOD escape_string.
    IF do_escaping EQ abap_false.
      escaped_string = input.
    ENDIF.
    escaped_string = cl_http_utility=>if_http_utility~escape_url( CONV #( input ) ).
  ENDMETHOD.

ENDCLASS.

