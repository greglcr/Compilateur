(*
  This file contains all the typing logic for expressions.
*)

open Ast
open TypedTree
open TyperCommon

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
      match SMap.find_opt name.spelling lenv with
      | Some typ -> mk_node typ e.expr_range (Texpr_variable name)
      | None ->
          (* We handle the variable "unit" differently because it is a
             special name for a constant of type unit. But unlike true
             or false, unit is not a keyword and can therefore be redefined
             by the user. *)
          if name.spelling <> "unit" then
            error e.expr_range ("unknown value " ^ name.spelling)
          else mk_node unit_type e.expr_range (Texpr_constant Cunit))
  | Ast.Pexpr_apply (name, args) -> (
      match Hashtbl.find_opt genv.func_decls name.spelling with
      | None -> error name.ident_range ("unknown value " ^ name.spelling)
      | Some func_decl ->
          (* Check the count of arguments. *)
          let args_count = List.length args in
          (if args_count <> func_decl.arity then
             let hint =
               Printf.sprintf "see %s type declaration at line %d" name.spelling
                 (fst func_decl.func_name.ident_range).lineno
             in
             let msg =
               Printf.sprintf
                 "function %s expected %d argument(s), but got %d argument(s)"
                 name.spelling func_decl.arity args_count
             in
             error_with_hint name.ident_range msg hint);

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
          mk_node return_type e.expr_range (Texpr_apply (func_decl, targs)))
  | Ast.Pexpr_constructor (name, args) -> (
      (* The typing of a constructor call is almost the same as a
         function apply expression. *)
      match Hashtbl.find_opt genv.cons_decls name.spelling with
      | None ->
          error name.ident_range ("unknown data constructor " ^ name.spelling)
      | Some cons_decl ->
          let args_count = List.length args in
          (if args_count <> cons_decl.arity then
             let hint =
               Printf.sprintf "see %s type declaration at line %d" name.spelling
                 (fst cons_decl.cons_name.ident_range).lineno
             in
             let msg =
               Printf.sprintf
                 "constructor %s expected %d argument(s), but got %d \
                  argument(s)"
                 name.spelling cons_decl.arity args_count
             in
             error_with_hint name.ident_range msg hint);

          let targs = List.map (type_expr genv lenv) args in
          let data_type = cons_decl.data_decl.data_typ in
          let _, cons_args_type =
            make_sigma cons_decl.data_decl.data_tvars
              (data_type :: cons_decl.args)
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
          mk_node return_type e.expr_range (Texpr_constructor (cons_decl, targs)))
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
          let lenv = SMap.add name.spelling tvalue.typ lenv in
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

      (*Then, check if the pattern of the branches are exhaustive*)

      (* list_typ contains a list of Ttyp_data *)

      check_exhaust genv tbranches;
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
  | Ast.Ppattern_variable { spelling = "_" } ->
      let typ = Ttyp_variable (V.create ()) in
      (lenv, mk_node typ pattern.pattern_range Tpattern_wildcard)
  | Ast.Ppattern_variable name ->
      let typ = Ttyp_variable (V.create ()) in
      ( SMap.add name.spelling typ lenv,
        mk_node typ pattern.pattern_range (Tpattern_variable name) )
  | Ast.Ppattern_constructor (name, patterns) -> (
      match Hashtbl.find_opt genv.cons_decls name.spelling with
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
      | None -> error name.ident_range ("unknown data constructor " ^ name.spelling))


(* To check exhaustivity, we implement the algorithm given in class*)
and check_exhaust genv tbranches =
  let patterns = List.fold_right (fun x acc -> let (pat, _) = x in [pat] :: acc) tbranches [] in

  let rec get_lines_with_wildcards_and_variables patterns = match patterns with
    | [] -> []
    | [] :: r_patterns -> get_lines_with_wildcards_and_variables r_patterns
    | ({typ = _; range = _; node = Tpattern_wildcard} :: r_pattern) :: r_patterns -> r_pattern :: (get_lines_with_wildcards_and_variables r_patterns)
    | ({typ = Ttyp_variable (_); range = _; node = _} :: r_pattern) :: r_patterns -> r_pattern :: (get_lines_with_wildcards_and_variables r_patterns)
    | _ :: r_patterns -> get_lines_with_wildcards_and_variables r_patterns

  and get_lines_with_true patterns = match patterns with
    | [] -> []
    | [] :: r_patterns -> get_lines_with_true r_patterns
    | ({typ = _; range = _; node = Tpattern_constant(Cbool(true))} :: r_pattern) :: r_patterns -> r_pattern :: (get_lines_with_true r_patterns)
    | _ :: r_patterns -> get_lines_with_true r_patterns

  and get_lines_with_false patterns = match patterns with
    | [] -> []
    | [] :: r_patterns -> get_lines_with_false r_patterns
    | ({typ = _; range = _; node = Tpattern_constant(Cbool(false))} :: r_pattern) :: r_patterns -> r_pattern :: (get_lines_with_false r_patterns)
    | _ :: r_patterns -> get_lines_with_false r_patterns

  and fill_new_matrices new_matrices data_info = List.iter (fun constructor -> Hashtbl.add new_matrices constructor.cons_name.spelling []) data_info.constructors

  and construct_new_matrice new_matrices construct_name patterns =
    let nb_arguments = (Hashtbl.find genv.cons_decls construct_name).arity in
    let rec type_patterns types patterns = match (types, patterns) with
      | ([], []) -> []
      | (tp :: r_types, pattern :: r_patterns) -> {typ = tp; range = ; node = pattern} :: (type_patterns r_types r_patterns)
      | _ -> assert(false) in
    List.iter 
      ( 
        fun pattern -> match pattern with
          | [] -> ()
          | ({typ = _; range = _; node = Tpattern_constructor(name, arguments)} :: r_pattern) when name.spelling = construct_name ->
            let new_list = arguments @ r_pattern in
            Hashtbl.replace new_matrices construct_name (new_list :: Hashtbl.find new_matrices construct_name);
          | ({typ = Ttyp_variable (_); range = _; node = _} :: r_pattern) ->
            let typed_arguments = type_patterns (Hashtbl.find genv.cons_decls construct_name).args (List.init nb_arguments (fun _ -> Tpattern_wildcard)) in
            let new_list = typed_arguments @ r_pattern in
            Hashtbl.replace new_matrices construct_name (new_list:: Hashtbl.find new_matrices construct_name);
          | ({typ = _; range = _; node = Tpattern_wildcard} :: r_pattern) ->
            let typed_arguments = type_patterns (Hashtbl.find genv.cons_decls construct_name).args (List.init nb_arguments (fun _ -> Tpattern_wildcard)) in
            let new_list = typed_arguments @ r_pattern in
            Hashtbl.replace new_matrices construct_name (new_list :: Hashtbl.find new_matrices construct_name);
          | _  -> ()
      )
      patterns in

  let rec aux patterns = match patterns with
    | [] -> error Location.dummy_range "Non exhaustive pattern"
    | [] :: _ -> () (*Final case of the algorithm*)
    | (pattern :: _) :: _ -> let typ_first_param = pattern.typ in (match (typ_first_param, patterns) with
      | (_, []) -> assert(false)
      | (typ_first_param, patterns) when typ_first_param = int_type -> aux (get_lines_with_wildcards_and_variables patterns);
      | (typ_first_param, patterns) when typ_first_param = string_type -> aux (get_lines_with_wildcards_and_variables patterns);
      | (typ_first_param, patterns) when typ_first_param = boolean_type -> aux ((get_lines_with_wildcards_and_variables patterns) @ (get_lines_with_true patterns));
                                                                           aux ((get_lines_with_wildcards_and_variables patterns) @ (get_lines_with_false patterns));
      | (Ttyp_data(name_data, _), patterns) ->
        let data_info = Hashtbl.find genv.data_decls name_data in
        let new_matrices = Hashtbl.create (List.length data_info.constructors) in
        fill_new_matrices new_matrices data_info;
        List.iter (fun construct -> construct_new_matrice new_matrices construct.cons_name.spelling patterns) data_info.constructors;
        List.iter (fun construct -> aux (Hashtbl.find new_matrices construct.cons_name.spelling)) data_info.constructors;
        ()
      | _ -> ()) in
  aux patterns
