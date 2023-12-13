open Typedtree
open Ast

exception Error of Location.range * string * string option

let error_with_maybe_hint range msg hint = raise (Error (range, msg, hint))
let error range msg = error_with_maybe_hint range msg None
let error_with_hint range msg hint = error_with_maybe_hint range msg (Some hint)

module SMap = Map.Make (String)

type global_env = {
  func_decls : (string, function_decl) Hashtbl.t;
  cons_decls : (string, constructor_decl) Hashtbl.t;
  data_decls : (string, data_decl) Hashtbl.t;
  class_decls : (string, class_decl) Hashtbl.t;
}

(*
There many simplications over the real PureScript about the typing:
  - There no type inference for functions signature. That is, a function
    declaration is always given along a function signature (eg. f :: T1 -> T2 -> T3).
  - Case expression with multiple patterns are not allowed:
        case e1, e2 of ...
    Therefore, functions using multiple patterns as arguments are not allowed.
*)

let rec head = function Ttyp_variable { def = Some t } -> head t | t -> t

let rec occur v t =
  match head t with Ttyp_variable w -> V.equal v w | _ -> false

exception UnificationFailure of Typedtree.typ * Typedtree.typ

let unification_error t1 t2 = raise (UnificationFailure (t1, t2))

(* Implements the type unification algorithm.
   See - https://en.wikipedia.org/wiki/Unification_(computer_science)
       - https://www.lri.fr/~filliatr/ens/compil/td/6/index.html *)
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

let rec pp_type fmt typ =
  match typ with
  | Ttyp_variable { id } -> Format.fprintf fmt "t%d" id
  | Ttyp_data (name, []) -> Format.fprintf fmt "%s" name
  | Ttyp_data (name, tvars) ->
      Format.fprintf fmt "%s @[%a@]" name (pp_list pp_type) tvars

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

(* $ can not be typed by the user so this identifier is always different from
    any source or user typed identifier. *)
let dummy_ident = { range = Location.dummy_range; node = "$" }
let fill_with_dummy_range node = { range = Location.dummy_range; node }

let type_of_constant c =
  match c with
  | Ast.Cbool _ -> boolean_type
  | Ast.Cint _ -> int_type
  | Ast.Cstring _ -> string_type
  | Ast.Cunit -> unit_type

let effect_unit = Ttyp_data ("Effect", [ unit_type ])
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

(* ========================================================
   Expression Typing
*)

(* Checks if the given type has the Eq type class defined for it.
   In other terms, returns true if typ can be compared using == or /=. *)
let is_equalable_type typ =
  let t = head typ in
  t = boolean_type || t = int_type || t = string_type

let rec type_expr genv lenv e =
  match e.expr_kind with
  | Ast.Pexpr_constant c ->
      mk_node (type_of_constant c) e.expr_range (Texpr_constant c)
  | Ast.Pexpr_binary (op, lhs, rhs) ->
      let tlhs = type_expr genv lenv lhs in
      let trhs = type_expr genv lenv rhs in
      let t =
        match op.node with
        | Ast.Badd | Ast.Bsub | Ast.Bmul | Ast.Bdiv ->
            unify_range tlhs.typ int_type tlhs.range;
            unify_range trhs.typ int_type trhs.range;
            int_type
        | Ast.Bconcat ->
            unify_range tlhs.typ string_type tlhs.range;
            unify_range trhs.typ string_type trhs.range;
            string_type
        | Ast.Beq | Ast.Bneq ->
            unify_range tlhs.typ trhs.typ tlhs.range;
            if is_equalable_type tlhs.typ then boolean_type
            else
              (* TODO *)
              let msg =
                Format.asprintf "type %a is not comparable for == and /="
                  pp_type tlhs.typ
              in
              error_with_hint tlhs.range msg
                "expected Boolean, Int or String type"
        | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge ->
            unify_range tlhs.typ int_type tlhs.range;
            unify_range trhs.typ int_type trhs.range;
            boolean_type
        | Ast.Bor | Ast.Band ->
            unify_range tlhs.typ boolean_type tlhs.range;
            unify_range trhs.typ boolean_type trhs.range;
            boolean_type
      in
      mk_node t e.expr_range (Texpr_binary (op, tlhs, trhs))
  | Ast.Pexpr_neg expr ->
      (* We transform "-e" to the equivalent code "0 - e". *)
      type_expr genv lenv
        {
          expr_range = e.expr_range;
          expr_kind =
            Ast.Pexpr_binary
              ( {
                  (* The binary operator. As we do not preserve the unary '-'
                     location in the AST, we can not "fake" the location of
                     the binary '-' operator. *)
                  range = Location.dummy_range;
                  node = Ast.Bsub;
                },
                {
                  (* The constant 0 does not exist in the source code.
                     Therefore, there is no "source range" for it and so
                     we just pass a dummy invalid range. *)
                  expr_range = Location.dummy_range;
                  expr_kind = Pexpr_constant (Cint 0);
                },
                expr );
        }
  | Ast.Pexpr_variable name -> (
      match SMap.find_opt name.node lenv with
      | Some typ -> mk_node typ e.expr_range (Texpr_variable name)
      | None ->
          (* We handle the variable "unit" differently because it is a
             special name for a constant of type unit. But unlike true
             or false, unit is not a keyword and can therefore be redefined
             by the user. *)
          if name.node <> "unit" then
            error e.expr_range ("unknown value " ^ name.node)
          else mk_node unit_type e.expr_range (Texpr_constant Cunit))
  | Ast.Pexpr_apply (name, args) -> (
      match Hashtbl.find_opt genv.func_decls name.node with
      | None -> error name.range ("unknown value " ^ name.node)
      | Some func_decl ->
          (* Check the count of arguments. *)
          let args_count = List.length args in
          (if args_count <> func_decl.arity then
             let hint =
               Printf.sprintf "see %s type declaration at line %d" name.node
                 (fst func_decl.func_name.range).lineno
             in
             let msg =
               Printf.sprintf
                 "function %s expected %d argument(s), but got %d argument(s)"
                 name.node func_decl.arity args_count
             in
             error_with_hint name.range msg hint);

          let targs = List.map (type_expr genv lenv) args in
          let _, func_args_type =
            make_sigma func_decl.tvars (func_decl.retty :: func_decl.params)
          in
          (* List.hd func_args_type is the function's return type and
             List.tl func_args_type are the function's arguments type. *)
          (* unify parameters *)
          List.iter2
            (fun targ expected_ty ->
              unify_range targ.typ expected_ty targ.range)
            targs (List.tl func_args_type);
          let return_type = Ttyp_variable (V.create ()) in
          unify_range (List.hd func_args_type) return_type e.expr_range;
          (* unify return type *)
          mk_node return_type e.expr_range (Texpr_apply (name, targs)))
  | Ast.Pexpr_constructor (name, args) -> (
      (* The typing of a constructor call is almost the same as a
         function apply expression. *)
      match Hashtbl.find_opt genv.cons_decls name.node with
      | None -> error name.range ("unknown data constructor " ^ name.node)
      | Some cons_decl ->
          let args_count = List.length args in
          (if args_count <> cons_decl.arity then
             let hint =
               Printf.sprintf "see %s type declaration at line %d" name.node
                 (fst cons_decl.cons_name.range).lineno
             in
             let msg =
               Printf.sprintf
                 "constructor %s expected %d argument(s), but got %d \
                  argument(s)"
                 name.node cons_decl.arity args_count
             in
             error_with_hint name.range msg hint);

          let targs = List.map (type_expr genv lenv) args in
          let data_type = cons_decl.data_decl.data_typ in
          let _, cons_args_type =
            make_sigma cons_decl.data_decl.tvars (data_type :: cons_decl.args)
          in
          (* List.hd cons_args_type is the constructor's return type and
             List.tl cons_args_type are the constructor's arguments type. *)
          (* unify parameters *)
          List.iter2
            (fun targ expected_ty ->
              unify_range targ.typ expected_ty targ.range)
            targs (List.tl cons_args_type);
          let return_type = Ttyp_variable (V.create ()) in
          unify_range (List.hd cons_args_type) return_type e.expr_range;
          (* unify return type *)
          mk_node return_type e.expr_range (Texpr_constructor (name, targs)))
  | Ast.Pexpr_if (cond, then_, else_) ->
      let tcond = type_expr genv lenv cond in
      let tthen = type_expr genv lenv then_ in
      let telse = type_expr genv lenv else_ in
      unify_range tcond.typ boolean_type tcond.range;
      unify_range tthen.typ telse.typ tthen.range;
      mk_node tthen.typ e.expr_range (Texpr_if (tcond, tthen, telse))
  | Ast.Pexpr_do exprs ->
      let texprs =
        List.map
          (fun (e : Ast.expr) ->
            let te = type_expr genv lenv e in
            unify_range te.typ effect_unit te.range;
            te)
          exprs
      in
      List.iter (fun t -> unify_range t.typ effect_unit t.range) texprs;
      mk_node effect_unit e.expr_range (Texpr_do texprs)
  | Ast.Pexpr_let (bindings, e) -> (
      (* We transform a let expression with multiple bindings to nested let
          expressions with each a single binding. So the following code:

              let a = e1, b = e2, c = e3 in e

          is transformed to:

              let a = e1 in
                  let b = e2 in
                      let c = e3 in
                          e *)
      match bindings with
      | [] -> type_expr genv lenv e
      | (name, value) :: r ->
          let tvalue = type_expr genv lenv value in
          let lenv = SMap.add name.node tvalue.typ lenv in
          let texpr =
            type_expr genv lenv
              { expr_range = e.expr_range; expr_kind = Ast.Pexpr_let (r, e) }
          in
          mk_node texpr.typ e.expr_range (Texpr_let ((name, tvalue), texpr)))
  | Pexpr_case (cond, branches) ->
      let v = Ttyp_variable (V.create ()) in
      let tcond = type_expr genv lenv cond in
      let tbranches = List.map (fun x -> type_branch genv lenv x) branches in
      (* First, check if all branches pattern's type is compatible to tcond's type. *)
      List.iter
        (fun x ->
          let p = fst x in
          unify_range p.typ tcond.typ p.range)
        tbranches;
      (* Then, check if all branches expressions have the same type. *)
      List.iter
        (fun x ->
          let e = snd x in
          unify_range e.typ v e.range)
        tbranches;
      mk_node v e.expr_range (Texpr_case (tcond, tbranches))

and type_branch genv lenv (pattern, expr) =
  let lenv, tpattern = type_pattern genv lenv pattern in
  let texpr = type_expr genv lenv expr in
  (tpattern, texpr)

and check_arity args expected_arity =
  List.compare_length_with args expected_arity = 0

and type_pattern genv lenv pattern =
  match pattern.pattern_kind with
  | Ast.Ppattern_constant c ->
      ( lenv,
        mk_node (type_of_constant c) pattern.pattern_range (Tpattern_constant c)
      )
  | Ast.Ppattern_variable { node = "_" } ->
      let typ = Ttyp_variable (V.create ()) in
      (lenv, mk_node typ pattern.pattern_range Tpattern_wildcard)
  | Ast.Ppattern_variable name ->
      let typ = Ttyp_variable (V.create ()) in
      ( SMap.add name.node typ lenv,
        mk_node typ pattern.pattern_range (Tpattern_variable name) )
  | Ast.Ppattern_constructor (name, patterns) -> (
      match Hashtbl.find_opt genv.cons_decls name.node with
      | Some cons_decl ->
          let tpatterns = List.map (type_pattern genv lenv) patterns in
          let lenv =
            List.fold_left
              (fun env1 env2 -> SMap.union (fun name _ t -> None) env1 env2)
              SMap.empty (List.map fst tpatterns)
          in
          ( lenv,
            mk_node cons_decl.data_decl.data_typ pattern.pattern_range
              (Tpattern_constructor (name, List.map snd tpatterns)) )
      | None -> error name.range ("unknown data constructor " ^ name.node))

let make_lenv_from_tvars tvars =
  let lenv = Hashtbl.create 17 in
  let tvars_type = ref [] in
  List.iter
    (fun tvar ->
      match Hashtbl.find_opt lenv tvar.node with
      | None ->
          let t = Ttyp_variable (V.create ()) in
          Hashtbl.add lenv tvar.node t;
          tvars_type := t :: !tvars_type
      | Some _ ->
          error tvar.range
            ("type argument " ^ tvar.node ^ " appears more than once"))
    tvars;
  (lenv, tvars_type)

(* ========================================================
   Function Typing
*)

(* Find all the consecutive function equations for the function named fname.
   This also returns all the remaining declarations that are not part of
   the function's equations. *)
let rec collect_equations (fname : string) (decls : decl list) =
  let rec loop equations decls =
    match decls with
    | [] -> (equations, [])
    | decl :: r -> (
        match decl.decl_kind with
        (* A function equation, just store it and continue the scanning. *)
        | Pdecl_equation (name, _, _) when name.node = fname ->
            loop (decl :: equations) r
        (* Either not a function with the same name or not a function at all.
           We found all the function equations. *)
        | _ -> (equations, decl :: r))
  in
  loop [] decls

let is_non_variable_pattern pattern =
  match pattern.pattern_kind with
  | Ppattern_variable _ -> false (* this also include the wildcard _ *)
  | _ -> true

(* Check if a function's equation is valid. *)
let type_function_equation genv decl (func_decl : function_decl) pattern_idx
    (name, patterns, body) =
  (* We start to check if the equation's arity is the same as
     the expected arity. *)
  let args_length = List.length patterns in
  (if args_length <> func_decl.arity then
     let hint =
       Format.asprintf "got %d, but type declaration requires %d" args_length
         func_decl.arity
     in
     error_with_hint name.range
       ("incorrect number of arguments for function " ^ name.node)
       hint);

  let lenv = ref SMap.empty in
  (* Then, we check if each pattern is correctly typed. *)
  let tpatterns =
    List.map2
      (fun pattern expected_type ->
        let _, typed_pattern = type_pattern genv !lenv pattern in
        unify_range typed_pattern.typ expected_type typed_pattern.range;
        (match typed_pattern.node with
        | Tpattern_variable v ->
            if Option.is_none (SMap.find_opt v.node !lenv) then
              lenv := SMap.add v.node typed_pattern.typ !lenv
            else
              error typed_pattern.range
                ("pattern variable " ^ v.node ^ " appears more than once")
        | _ -> ());
        typed_pattern)
      patterns func_decl.params
  in
  let tbody = type_expr genv !lenv body in
  unify_range tbody.typ func_decl.retty body.expr_range;

  (* We also check that there is at most one non-variable pattern
     and it is at the same position as the other non-variables patterns
     in the other equations. *)
  List.iteri
    (fun i pattern ->
      if is_non_variable_pattern pattern then
        if Option.is_none !pattern_idx then pattern_idx := Some i
        else if Option.get !pattern_idx <> i then
          let msg =
            Printf.sprintf "non variable pattern at the %d-th argument" i
          in
          let hint =
            Printf.sprintf
              "but a non variable pattern was first encountered at the %d-th \
               argument"
              (Option.get !pattern_idx)
          in
          error_with_hint pattern.pattern_range msg hint)
    patterns;

  (tpatterns, tbody)

let rec from_ast_type genv lenv (typ : Ast.typ) =
  match typ.type_kind with
  | Ptyp_variable v -> (
      match Hashtbl.find_opt lenv v with
      | Some t -> t
      | None -> error typ.type_range ("type variable " ^ v ^ " is undefined"))
  | Ptyp_data (name, args) -> (
      match Hashtbl.find_opt genv.data_decls name.node with
      | Some { arity } ->
          let args_count = List.length args in
          (if args_count <> arity then
             let hint =
               Format.asprintf "got %d, but %d is required" args_count arity
             in
             error_with_hint name.range
               ("incorrect number of type variables for data " ^ name.node)
               hint);

          Ttyp_data (name.node, List.map (from_ast_type genv lenv) args)
      | None -> error name.range ("unknown type " ^ name.node))

let rec last_element lst =
  match lst with
  | [] -> failwith "Empty list has no last element"
  | [ x ] -> x
  | _ :: rest -> last_element rest

let mk_func_decl func_name tvars params retty =
  { func_name; tvars; params; retty; arity = List.length params }

let type_function genv name (decl : Ast.decl) (equations : Ast.decl list) =
  if equations = [] then
    error decl.decl_range
      ("type declaration of " ^ name.node
     ^ " should be followed by its definition");

  let func_decl =
    match decl.decl_kind with
    | Pdecl_function (_, tvars, _, params, retty) ->
        let lenv, _ = make_lenv_from_tvars tvars in
        let tparams = List.map (from_ast_type genv lenv) params in
        let tretty = from_ast_type genv lenv retty in
        mk_func_decl name lenv tparams tretty
    | _ -> assert false
  in
  Hashtbl.add genv.func_decls name.node func_decl;

  (* Check if all function's equations are correctly typed. *)
  let pattern_idx = ref None in
  let tequations =
    List.map
      (fun eq ->
        match eq.decl_kind with
        | Pdecl_equation (name, patterns, body) ->
            type_function_equation genv decl func_decl pattern_idx
              (name, patterns, body)
        | _ -> failwith "unexpected")
      equations
  in

  (* And if yes, compress all of them into a single function expression.
     In other words, if we have
          f p1 = e1
          f p2 = e2
          ...
          f pn = en
     with p1, ..., pn n patterns and e1, ..., en n expressions.
     Then we transform that into the following equivalent code:
          f x =
              case x of
                  p1 -> e1
                  p2 -> e2
                  ...
                  pn -> en *)
  let function_params, function_body =
    if name.node = "main" then
      (* The main function is managed separately because we don't want to
         have several equations for it. *)
      if List.length tequations <> 1 then
        error name.range "multiple value declarations exist for main"
      else ([], snd (List.hd tequations))
    else if Option.is_none !pattern_idx then
      (* All arguments are variable patterns. This case is easy, we don't need
         to convert all equations to a single case expression. If we have
         multiple equations, we only keep the last one. *)
      let patterns, body = last_element tequations in
      ( List.map
          (fun (pattern : Typedtree.pattern) ->
            match pattern.node with
            | Tpattern_wildcard -> (fill_with_dummy_range "", pattern.typ)
            | Tpattern_variable x -> (x, pattern.typ)
            | _ -> assert false)
          patterns,
        body )
    else
      let pattern_idx = Option.get !pattern_idx in
      let pattern_type = List.nth func_decl.params pattern_idx in
      let pattern_argument =
        mk_node pattern_type Location.dummy_range
          (Tpattern_variable dummy_ident)
      in
      let branches =
        List.map
          (fun (patterns, expr) -> (List.nth patterns pattern_idx, expr))
          tequations
      in
      (* FIXME: one should replace the old parameters references in each
         equations to the new parameters of the "compressed function". For example,
              f x true = x
              f y false = y
         should be converted to
              f x $ = case $ of
                  true -> x
                  false -> x
         and not
              f x $ case $ of
                  true -> x
                  false -> y !! y does not exist anymore

         However, this does not affect typing but will be problematic later
         for compilation. *)
      ( [],
        mk_node func_decl.retty Location.dummy_range
          (Texpr_case
             ( mk_node pattern_type Location.dummy_range
                 (Texpr_variable dummy_ident),
               branches )) )
  in

  (* FIXME: remove function_type *)
  let function_type = Ttyp_data ("", []) in
  mk_node function_type decl.decl_range
    (Tdecl_function (name, function_params, function_body))

(* ========================================================
       DATA TYPING
   ======================================================== *)

let previous_declaration_hint (range : Location.range) =
  let lineno = (fst range).lineno in
  if lineno <> 0 then
    let hint = Format.sprintf "previous declaration is at line %d" lineno in
    Some hint
  else None

let merge_ht ~into:tab1 tab2 =
  Hashtbl.fold (fun key elt () -> Hashtbl.replace tab1 key elt) tab2 ();
  tab1

let mk_constructor_decl cons_name data_decl args =
  { cons_name; data_decl; args; arity = List.length args }

let type_data_constructor genv lenv data_decl (name, args) =
  let targs = List.map (from_ast_type genv lenv) args in

  match Hashtbl.find_opt genv.cons_decls name.node with
  | None ->
      let cons_decl = mk_constructor_decl name data_decl targs in
      Hashtbl.add genv.cons_decls name.node cons_decl;
      cons_decl
  | Some previous_decl ->
      let hint = previous_declaration_hint previous_decl.cons_name.range in
      error_with_maybe_hint name.range
        ("redeclaration of data constructor " ^ name.node)
        hint

let type_data genv data_name range tvars constructors =
  let lenv, tvars_type = make_lenv_from_tvars tvars in

  let data_typ = Ttyp_data (data_name.node, !tvars_type) in
  let data_decl =
    {
      data_name;
      constructors = [];
      tvars = lenv;
      arity = List.length tvars;
      data_typ;
    }
  in
  Hashtbl.add genv.data_decls data_name.node data_decl;

  List.map (type_data_constructor genv lenv data_decl) constructors

(* ========================================================
       CLASS TYPING
   ======================================================== *)

(* Type and check a function declaration found inside a class declaration.
   This function will add the function declaration to the gloval environnement. *)
let type_class_member genv lenv decl =
  match decl.decl_kind with
  | Pdecl_function (name, tvars, instances, params, retty) -> (
      match Hashtbl.find_opt genv.func_decls name.node with
      | None ->
          let func_lenv, _ = make_lenv_from_tvars tvars in
          let lenv = merge_ht ~into:func_lenv lenv in
          let tparams = List.map (from_ast_type genv lenv) params in
          let tretty = from_ast_type genv lenv retty in
          let func_decl = mk_func_decl name lenv tparams tretty in
          Hashtbl.add genv.func_decls name.node func_decl
      | Some previous_decl ->
          let hint = previous_declaration_hint previous_decl.func_name.range in
          error_with_maybe_hint name.range
            ("redeclaration of value " ^ name.node)
            hint)
  | _ -> assert false

let mk_class_decl class_name tvars = { class_name; tvars }

(* Type a class declaration and register it into the global environnement
   with all its functions. *)
let type_class genv name range tvars fields =
  let lenv, _ = make_lenv_from_tvars tvars in
  let class_decl = mk_class_decl name lenv in
  Hashtbl.add genv.class_decls name.node class_decl;
  List.iter (type_class_member genv lenv) fields

(* ========================================================
       PROGRAM TYPING
   ======================================================== *)

let mk_builtin_function name params retty =
  {
    func_name = fill_with_dummy_range name;
    tvars = Hashtbl.create 0;
    params;
    retty;
    arity = List.length params;
  }

let mk_builtin_data_ty name ty arity =
  {
    data_name = fill_with_dummy_range name;
    data_typ = ty;
    arity;
    constructors = [];
    tvars = Hashtbl.create 0;
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

let file decls =
  let genv = default_genv in
  (* Collect all declarations: *)
  let rec loop acc decls =
    match decls with
    | [] -> acc
    | decl :: r -> (
        match decl.decl_kind with
        | Pdecl_equation (name, _, _) ->
            error name.range ("value " ^ name.node ^ " has no type declaration")
        | Pdecl_function (name, _, _, _, _) -> (
            match Hashtbl.find_opt genv.func_decls name.node with
            | None ->
                let equations, next_decls = collect_equations name.node r in
                let tdecl = type_function genv name decl equations in
                loop (tdecl :: acc) next_decls
            | Some previous_decl ->
                let hint =
                  previous_declaration_hint previous_decl.func_name.range
                in
                error_with_maybe_hint name.range
                  ("redeclaration of value " ^ name.node)
                  hint)
        | Pdecl_data (name, tvars, constructors) -> (
            match Hashtbl.find_opt genv.data_decls name.node with
            | None ->
                type_data genv name decl.decl_range tvars constructors;
                loop acc r
            | Some previous_decl ->
                let hint =
                  previous_declaration_hint previous_decl.data_name.range
                in
                error_with_maybe_hint name.range
                  ("redeclaration of data type " ^ name.node)
                  hint)
        | Pdecl_class (name, tvars, fields) -> (
            match Hashtbl.find_opt genv.class_decls name.node with
            | None ->
                type_class genv name decl.decl_range tvars fields;
                loop acc r
            | Some previous_decl ->
                let hint =
                  previous_declaration_hint previous_decl.class_name.range
                in
                error_with_maybe_hint name.range
                  ("redeclaration of class type " ^ name.node)
                  hint)
        | Pdecl_instance (_, _) -> loop acc r)
  in
  let tdecls = loop [] decls in

  if Option.is_none (Hashtbl.find_opt genv.func_decls "main") then
    error Location.dummy_range "value main is missing"
  else tdecls

(* --------------------------------------------------------
   Pattern Matching Typing
*)

(*
    We implement the algorithm described here:
        http://moscova.inria.fr/~maranget/papers/warn
    Based on explanations from here:
        https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_build/thir/pattern/usefulness/index.html
*)

(* type specialize_constructor = SCconstant of Ast.constant | SCdata of constructor_decl

   let specialize c p = match p.node with
       | Tpattern_constant constant1 ->(
           match c with
               | SCconstant constant2 when constant1 = constant2 -> Some []
               | _ -> None)
       | Tpattern_constructor (name, args) -> (
           match c with
               | SCdata constructor when constructor.name.node = name.node -> Some []
               | _ -> None)
       | _ -> None

   (* Returns the first k elements of a list. *)
   let rec firstk k xs = match xs with
       | [] -> failwith "firstk"
       | x::xs ->
           if k=1 then [x], xs
           else let x', xs' = firstk (k-1) xs in x::x', xs'

   let unspecialize c p = match c with
       | SCconstant constant -> (make_node (type_of_constant constant) Location.dummy_range (Tpattern_constant constant)) :: p
       | SCdata constructor -> (
           let args, p = firstk constructor.arity p in
           (make_node constructor.parent_type Location.dummy_range (Tpattern_constructor (constructor.name, args))) :: p
       )

   let rec usefulness patterns typ q = match typ with
       | [] ->
           if patterns = [] then
               Some []
           else
               None

       | _ ->
           List.filter_map (fun x -> x) (List.map (fun c -> specialize q) [CSconstant (Cunit)]) *)
