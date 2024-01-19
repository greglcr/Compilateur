(*
  This file contains many utilities functions and common functions to all the
  typing logic.
*)

open Ast
open TypedTree
module SMap = Map.Make (String)

(* Errors management. *)
exception TypingError of Location.range * string * string option

let error_with_maybe_hint range msg hint =
  raise (TypingError (range, msg, hint))

let error range msg = error_with_maybe_hint range msg None
let error_with_hint range msg hint = error_with_maybe_hint range msg (Some hint)

(* The global environnement. *)
type global_env = {
  func_decls : (string, function_decl) Hashtbl.t;
  cons_decls : (string, constructor_decl) Hashtbl.t;
  data_decls : (string, data_decl) Hashtbl.t;
  class_decls : (string, class_decl) Hashtbl.t;
}

(* The following functions implements the unification algorithm.
   See - https://en.wikipedia.org/wiki/Unification_(computer_science)
       - https://www.lri.fr/~filliatr/ens/compil/td/6/index.html *)

let rec head = function Ttyp_variable { def = Some t } -> head t | t -> t

let rec occur v t =
  match head t with Ttyp_variable w -> V.equal v w | _ -> false

exception UnificationFailure of TypedTree.typ * TypedTree.typ

let unification_error t1 t2 = raise (UnificationFailure (t1, t2))

let rec unify t1 t2 =
  match (head t1, head t2) with
  | Ttyp_variable v1, Ttyp_variable v2 when V.equal v1 v2 -> ()
  | (Ttyp_variable v1 as t1), t2 ->
      if occur v1 t2 then unification_error t1 t2;
      assert (v1.def = None);
      v1.def <- Some t2
  | t1, (Ttyp_variable v2 as t2) -> unify t2 t1
  | Ttyp_data (name1, args1), Ttyp_data (name2, args2) when name1 = name2 ->
      unify_params args1 args2
  | t1, t2 -> unification_error t1 t2

(*We need another unify function that check that type t1 is more general than t2, useful for function declaration*)

and unify_strong t1 t2 = 
    match (head t1, head t2) with
    | Ttyp_variable v1, Ttyp_variable v2 when V.equal v1 v2 -> ()
    | (Ttyp_variable v1 as t1), t2 ->
        if occur v1 t2 then unification_error t1 t2;
        assert (v1.def = None);
        v1.def <- Some t2
    | Ttyp_data (name1, args1), Ttyp_data (name2, args2) when name1 = name2 ->
        unify_params args1 args2
    | t1, t2 -> unification_error t1 t2


and unify_params p1 p2 =
  match (p1, p2) with
  | [], [] -> ()
  | t1 :: r1, t2 :: r2 ->
      unify t1 t2;
      unify_params r1 r2
  | [], _ | _, [] -> failwith "arity was already checked"

let rec pp_list pp_v fmt = function
  | [] -> ()
  | [ x ] -> pp_v fmt x
  | x :: xs -> Format.fprintf fmt "%a,@ %a" pp_v x (pp_list pp_v) xs

(* Type pretty printer for error reporting: *)
let rec pp_type fmt typ =
  match typ with
  | Ttyp_variable { id } -> Format.fprintf fmt "t%d" id
  | Ttyp_data (name, []) -> Format.fprintf fmt "%s" name
  | Ttyp_data (name, tvars) ->
      Format.fprintf fmt "%s @[%a@]" name (pp_list pp_type) tvars

and pp_func_decl_forall fmt tvars =
  if Hashtbl.length tvars > 0 then (
    Format.fprintf fmt " forall";
    Hashtbl.iter (fun _ t -> Format.fprintf fmt " %a" pp_type t) tvars;
    Format.fprintf fmt ".")

and pp_func_decl fmt func_decl =
  Format.fprintf fmt "%s ::%a %a -> %a" func_decl.func_name.spelling
    pp_func_decl_forall func_decl.tvars (pp_list pp_type) func_decl.params
    pp_type func_decl.retty

(* Utility function around unify to emit a proper error in case
   of unification failure with the given source location. *)
let unify_range t1 t2 range =
  try unify t1 t2
  with UnificationFailure (t1, t2) ->
    let msg =
      Format.asprintf "impossible to unify type %a with type %a" pp_type t1
        pp_type t2
    in
    error range msg

and unify_range_strong t1 t2 range = 
  try unify_strong t1 t2
  with UnificationFailure (t1, t2) ->
    let msg =
      Format.asprintf "impossible to unify type %a with type %a" pp_type t1
        pp_type t2
    in
    error range msg


(* $ can not be typed by the user so this identifier is always different from
    any source or user typed identifier. *)
let dummy_ident = { ident_range = Location.dummy_range; spelling = "$" }

let fill_with_dummy_range spelling =
  { ident_range = Location.dummy_range; spelling }

let type_of_constant c =
  match c with
  | Ast.Cbool _ -> boolean_type
  | Ast.Cint _ -> int_type
  | Ast.Cstring _ -> string_type
  | Ast.Cunit -> unit_type

let mk_node typ range node = { typ; range; node }

let make_sigma tvars args =
  (*tvars = list of variable typ*)
  let new_tvars = Hashtbl.create 17 in
  let ht = Hashtbl.create 17 in
  Hashtbl.iter
    (fun name (Ttyp_variable t) ->
      Hashtbl.add new_tvars name (V.create ());
      Hashtbl.add ht t.id name)
    tvars;
  let rec explore_type current_typ =
    match current_typ with
    | Ttyp_variable next_type -> (
        match next_type.def with
        | None ->
            let name = Hashtbl.find ht next_type.id in
            Ttyp_variable (Hashtbl.find new_tvars name)
        | Some t ->
            let newV = V.create () in
            newV.def <- Some (explore_type t);
            Ttyp_variable newV)
    | Ttyp_data (name, args) -> Ttyp_data (name, List.map explore_type args)
  in
  let new_args = List.map explore_type args in
  (new_tvars, new_args)

(* Find all the consecutive function equations for the function named fname.
   This also returns all the remaining declarations that are not part of
   the function's equations. *)
let rec collect_equations fname decls =
  let rec loop equations decls =
    match decls with
    | [] -> (equations, [])
    | decl :: r -> (
        match decl.decl_kind with
        (* A function equation, just store it and continue the scanning. *)
        | Pdecl_equation (name, _, _) when name.spelling = fname ->
            loop (decl :: equations) r
        (* Either not a function with the same name or not a function at all.
           We found all the function equations. *)
        | _ -> (equations, decl :: r))
  in
  loop [] decls

let mk_func_decl func_name tvars params retty =
  { func_name; tvars; params; retty; arity = List.length params }

let make_lenv_from_tvars tvars =
  let lenv = Hashtbl.create 17 in
  let tvars_type = ref [] in
  List.iter
    (fun tvar ->
      match Hashtbl.find_opt lenv tvar.spelling with
      | None ->
          let t = Ttyp_variable (V.create ()) in
          Hashtbl.add lenv tvar.spelling t;
          tvars_type := t :: !tvars_type
      | Some _ ->
          error tvar.ident_range
            ("type argument " ^ tvar.spelling ^ " appears more than once"))
    tvars;
  (lenv, tvars_type)

let rec from_ast_type genv lenv (typ : Ast.typ) =
  match typ.type_kind with
  | Ptyp_variable v -> (
      match Hashtbl.find_opt lenv v with
      | Some t -> t
      | None -> error typ.type_range ("type variable " ^ v ^ " is undefined"))
  | Ptyp_data (name, args) -> (
      match Hashtbl.find_opt genv.data_decls name.spelling with
      | Some { data_arity } ->
          let args_count = List.length args in
          (if args_count <> data_arity then
             let hint =
               Format.asprintf "got %d, but %d is required" args_count
                 data_arity
             in
             error_with_hint name.ident_range
               ("incorrect number of type variables for data " ^ name.spelling)
               hint);

          Ttyp_data (name.spelling, List.map (from_ast_type genv lenv) args)
      | None -> error name.ident_range ("unknown type " ^ name.spelling))

let previous_declaration_hint (range : Location.range) =
  let lineno = (fst range).lineno in
  if lineno <> 0 then
    let hint = Format.sprintf "previous declaration is at line %d" lineno in
    Some hint
  else None

let merge_ht ~into:tab1 tab2 =
  Hashtbl.fold (fun key elt () -> Hashtbl.replace tab1 key elt) tab2 ();
  tab1
