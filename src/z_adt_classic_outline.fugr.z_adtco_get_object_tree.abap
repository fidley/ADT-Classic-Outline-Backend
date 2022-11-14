FUNCTION z_adtco_get_object_tree.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(OBJECT_NAME) TYPE  EU_LNAME
*"     VALUE(OBJECT_TYPE) TYPE  SEU_OBJ
*"     VALUE(PARAMETERS) TYPE  TIHTTPNVP OPTIONAL
*"  EXPORTING
*"     VALUE(TREE) TYPE  SNODETAB
*"----------------------------------------------------------------------
  tree = NEW zcl_adtco_tree_creator( )->create_tree( object_name = object_name
                                                     object_type = object_type
                                                     parameters = parameters ).





ENDFUNCTION.
