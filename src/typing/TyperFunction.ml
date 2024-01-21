(*
  This file contains all the logic to type function declarations and equations.
*)

open Ast
open TypedTree
open TyperCommon
open TyperExpression

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
     error_with_hint name.ident_range
       ("incorrect number of arguments for function " ^ name.spelling)
       hint);

  let lenv = ref SMap.empty in
  (* Then, we check if each pattern is correctly typed. *)
  let tpatterns =
    let rec add_to_lenv_in_depth typed_pattern = match typed_pattern.node with
      | Tpattern_variable v ->
        if Option.is_none (SMap.find_opt v.spelling !lenv) then
          lenv := SMap.add v.spelling typed_pattern.typ !lenv
        else
          error typed_pattern.range
            ("pattern variable " ^ v.spelling ^ " appears more than once")
      | Tpattern_constructor(name, patterns) -> List.iter (fun pattern -> add_to_lenv_in_depth pattern) patterns; ()
      | _ -> () (* For Tpattern_wildcard and Tpattern_constant *) in
    List.map2
      (fun pattern expected_type ->
        let _, typed_pattern = type_pattern genv !lenv pattern in
        unify_range_strong typed_pattern.typ expected_type typed_pattern.range;
        add_to_lenv_in_depth typed_pattern;
        typed_pattern)
        (*On a les pattern qui sont typé. Le problème, c'est qu'on va pas rentrer assez en profondeur pour typer toutes les variables dans les patterns
           Il faut donc faire un truc récursif un peu comme dans l'exhaustivité des patterns matching*)
      patterns func_decl.params
  in
  let tbody = type_expr genv !lenv body in
  unify_range_strong tbody.typ func_decl.retty body.expr_range;

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

let rec last_element lst =
  match lst with
  | [] -> failwith "Empty list has no last element"
  | [ x ] -> x
  | _ :: rest -> last_element rest

let type_function genv name decl equations =
  if equations = [] then
    error decl.decl_range
      ("type declaration of " ^ name.spelling
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
  Hashtbl.add genv.func_decls name.spelling func_decl;

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
    if name.spelling = "main" then
      (* The main function is managed separately because we don't want to
         have several equations for it. *)
      if List.length tequations <> 1 then
        error name.ident_range "multiple value declarations exist for main"
      else ([], snd (List.hd tequations))
    else if Option.is_none !pattern_idx then
      (* All arguments are variable patterns. This case is easy, we don't need
         to convert all equations to a single case expression. If we have
         multiple equations, we only keep the last one. *)
      let patterns, body = last_element tequations in
      ( List.map
          (fun (pattern : TypedTree.pattern) ->
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
      check_exhaust genv branches;
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
