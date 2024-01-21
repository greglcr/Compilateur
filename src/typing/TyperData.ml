(*
  This file contains all the logic to type data declarations.
*)

open Ast
open TypedTree
open TyperCommon

let mk_constructor_decl i cons_name data_decl args =
  { const_discriminant = i; cons_name; data_decl; args; arity = List.length args }

let type_data_constructor genv lenv data_decl i (name, args) =
  let targs = List.map (from_ast_type genv lenv) args in

  match Hashtbl.find_opt genv.cons_decls name.spelling with
  | None ->
      let cons_decl = mk_constructor_decl i name data_decl targs in
      Hashtbl.add genv.cons_decls name.spelling cons_decl;
      cons_decl
  | Some previous_decl ->
      let hint =
        previous_declaration_hint previous_decl.cons_name.ident_range
      in
      error_with_maybe_hint name.ident_range
        ("redeclaration of data constructor " ^ name.spelling)
        hint

let mk_data_decl data_name data_tvars data_typ =
  {
    data_name;
    constructors = [];
    data_tvars;
    data_typ;
    data_arity = Hashtbl.length data_tvars;
  }

let type_data genv data_name range tvars constructors =
  let lenv, tvars_type = make_lenv_from_tvars tvars in

  let data_typ = Ttyp_data (data_name.spelling, !tvars_type) in
  let data_decl = mk_data_decl data_name lenv data_typ in

  Hashtbl.add genv.data_decls data_name.spelling data_decl;
  let typed_constructors = List.mapi (type_data_constructor genv lenv data_decl) constructors in
  data_decl.constructors <- typed_constructors;
  Hashtbl.add genv.data_decls data_name.spelling data_decl;
