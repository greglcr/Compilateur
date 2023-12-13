(*
  This file contains all the logic to type class and instance declarations.
*)

open Ast
open TypedTree
open TyperCommon

(* ========================================================
       CLASS TYPING
   ======================================================== *)

(* Type and check a function declaration found inside a class declaration.
   This function will add the function declaration to the gloval environnement.
   The function's name is returned. *)
let type_class_member genv lenv decl =
  match decl.decl_kind with
  | Pdecl_function (name, tvars, instances, params, retty) -> (
      match Hashtbl.find_opt genv.func_decls name.spelling with
      | None ->
          let func_lenv, _ = make_lenv_from_tvars tvars in
          let lenv = merge_ht ~into:func_lenv lenv in
          let tparams = List.map (from_ast_type genv lenv) params in
          let tretty = from_ast_type genv lenv retty in
          let func_decl = mk_func_decl name lenv tparams tretty in
          Hashtbl.add genv.func_decls name.spelling func_decl;
          name.spelling
      | Some previous_decl ->
          let hint =
            previous_declaration_hint previous_decl.func_name.ident_range
          in
          error_with_maybe_hint name.ident_range
            ("redeclaration of value " ^ name.spelling)
            hint)
  | _ -> assert false

let mk_class_decl class_name class_tvars class_funcs =
  { class_name; class_tvars; class_funcs }

(* Type a class declaration and register it into the global environnement
   with all its functions. *)
let type_class genv name range tvars members =
  let lenv, _ = make_lenv_from_tvars tvars in
  let funcs =
    List.fold_left
      (fun acc mem -> SSet.add (type_class_member genv lenv mem) acc)
      SSet.empty members
  in
  let class_decl = mk_class_decl name lenv funcs in
  Hashtbl.add genv.class_decls name.spelling class_decl

(* ========================================================
       INSTANCE TYPING
   ======================================================== *)

let type_instance_member genv lenv equations = ()

(* Prints all functions that were required but not implemented by the instance. *)
let check_instance_funcs_diff genv range diff_funcs =
  if not (SSet.is_empty diff_funcs) then (
    let hint = Buffer.create 1024 in
    Buffer.add_string hint
      "the following type class members have not been implemented:\n";
    SSet.iter
      (fun func_name ->
        let func_decl = Hashtbl.find genv.func_decls func_name in
        let signature = Format.asprintf "  %a\n" pp_func_decl func_decl in
        Buffer.add_string hint signature)
      diff_funcs;
    error_with_hint range "type class instance is missing some members"
      (Buffer.contents hint))

let type_instance genv (schema, (class_name, class_args)) funcs =
  match Hashtbl.find_opt genv.class_decls class_name.spelling with
  | None ->
      error class_name.ident_range ("unknown type class " ^ class_name.spelling)
  | Some class_decl ->
      let lenv = Hashtbl.create 0 in
      let args = List.map (from_ast_type genv lenv) class_args in
      (* Type each of the instance's members. *)
      let rec loop declared_funcs decls =
        match decls with
        | [] -> declared_funcs
        | func_decl :: r -> (
            match func_decl.decl_kind with
            | Pdecl_equation (name, _, _) -> (
                match SSet.find_opt name.spelling declared_funcs with
                | None ->
                    (* Lets collect all equations defining the function and type it. *)
                    let equations, next_decls =
                      collect_equations name.spelling (func_decl :: r)
                    in
                    type_instance_member genv lenv equations;
                    loop (SSet.add name.spelling declared_funcs) next_decls
                | Some _ ->
                    (* Multiple declarations of the function inside the instance... *)
                    error name.ident_range
                      ("redeclaration of value " ^ name.spelling))
            | _ -> assert false)
      in
      let declared_funcs = loop SSet.empty funcs in
      (* Check if we implemented all expected functions as required by the class type. *)
      let diff_funcs = SSet.diff class_decl.class_funcs declared_funcs in
      check_instance_funcs_diff genv class_name.ident_range diff_funcs
