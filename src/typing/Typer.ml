open Ast
open TypedTree
open TyperCommon
open TyperExpression
open TyperFunction
open TyperData
open TyperClass

(* ========================================================
       FILE TYPING
   ======================================================== *)

let mk_builtin_function name params retty =
  {
    func_name = fill_with_dummy_range name;
    tvars = Hashtbl.create 0;
    params;
    retty;
    arity = List.length params;
  }

let mk_builtin_data_ty name data_typ data_arity =
  {
    data_name = fill_with_dummy_range name;
    data_typ;
    data_arity;
    constructors = [];
    data_tvars = Hashtbl.create 0;
  }

let default_genv =
  let func_decls = Hashtbl.create 17 in
  let cons_decls = Hashtbl.create 17 in
  let data_decls = Hashtbl.create 17 in
  let class_decls = Hashtbl.create 17 in

  Hashtbl.add data_decls "Unit" (mk_builtin_data_ty "Unit" unit_type 0);
  Hashtbl.add data_decls "Boolean" (mk_builtin_data_ty "Boolean" boolean_type 0);
  Hashtbl.add data_decls "Int" (mk_builtin_data_ty "Int" int_type 0);
  Hashtbl.add data_decls "String" (mk_builtin_data_ty "String" string_type 0);
  Hashtbl.add data_decls "Effect" (mk_builtin_data_ty "Effect" effect_type 1);

  (* the constant unit is handled separately in type_expr *)
  Hashtbl.add func_decls "not"
    (mk_builtin_function "not" [ boolean_type ] boolean_type);
  Hashtbl.add func_decls "mod"
    (mk_builtin_function "mod" [ int_type; int_type ] int_type);
  Hashtbl.add func_decls "log"
    (mk_builtin_function "log" [ string_type ] effect_unit);

  let b = Ttyp_variable (V.create ()) in
  let tvars = Hashtbl.create 1 in
  Hashtbl.add tvars "b" b;
  Hashtbl.add func_decls "show"
    {
      func_name = fill_with_dummy_range "show";
      tvars;
      params = [ b ];
      retty = string_type;
      arity = 1;
    };

  let a = Ttyp_variable (V.create ()) in
  let tvars = Hashtbl.create 1 in
  Hashtbl.add tvars "a" a;
  Hashtbl.add func_decls "pure"
    {
      func_name = fill_with_dummy_range "pure";
      tvars;
      params = [ a ];
      retty = Ttyp_data ("Effect", [ a ]);
      arity = 1;
    };

  { func_decls; cons_decls; data_decls; class_decls }

let type_file decls =
  let genv = default_genv in
  let funcs = ref [] in
  (* Collect all declarations: *)
  let rec loop decls =
    match decls with
    | [] -> ()
    | decl :: r -> (
        match decl.decl_kind with
        | Pdecl_equation (name, _, _) ->
            error name.ident_range
              ("value " ^ name.spelling ^ " has no type declaration")
        | Pdecl_function (name, _, _, _, _) -> (
            match Hashtbl.find_opt genv.func_decls name.spelling with
            | None ->
                let equations, next_decls = collect_equations name.spelling r in
                funcs := type_function genv name decl equations :: !funcs;
                loop next_decls
            | Some previous_decl ->
                let hint =
                  previous_declaration_hint previous_decl.func_name.ident_range
                in
                error_with_maybe_hint name.ident_range
                  ("redeclaration of value " ^ name.spelling)
                  hint)
        | Pdecl_data (name, tvars, constructors) -> (
            match Hashtbl.find_opt genv.data_decls name.spelling with
            | None ->
                type_data genv name decl.decl_range tvars constructors;
                loop r
            | Some previous_decl ->
                let hint =
                  previous_declaration_hint previous_decl.data_name.ident_range
                in
                error_with_maybe_hint name.ident_range
                  ("redeclaration of data type " ^ name.spelling)
                  hint)
        | Pdecl_class (name, tvars, fields) -> (
            match Hashtbl.find_opt genv.class_decls name.spelling with
            | None ->
                type_class genv name decl.decl_range tvars fields;
                loop r
            | Some previous_decl ->
                let hint =
                  previous_declaration_hint previous_decl.class_name.ident_range
                in
                error_with_maybe_hint name.ident_range
                  ("redeclaration of class type " ^ name.spelling)
                  hint)
        | Pdecl_instance (schema, funcs) ->
            type_instance genv schema funcs;
            loop r)
  in
  loop decls;

  if Option.is_none (Hashtbl.find_opt genv.func_decls "main") then
    error Location.dummy_range "value main is missing";

  !funcs
