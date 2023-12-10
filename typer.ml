open Typedtree
open Ast

exception Error of Location.range * string * string option
let error range msg = raise (Error (range, msg, None))
let error_with_hint range msg hint = raise (Error (range, msg, Some hint))

module SMap = Map.Make(String)

type global_env =
    {
        functions : (string, function_decl) Hashtbl.t;
        constructors : (string, constructor_decl) Hashtbl.t;
        data_types : (string, data_decl) Hashtbl.t;
        class_types : (string, Typedtree.typ) Hashtbl.t;
    }

(*
There many simplications over the real PureScript about the typing:
  - There no type inference for functions signature. That is, a function
    declaration is always given along a function signature (eg. f :: T1 -> T2 -> T3).
  - Case expression with multiple patterns are not allowed:
        case e1, e2 of ...
    Therefore, functions using multiple patterns as arguments are not allowed.
*)

let rec head = function
    | Ttyp_variable { def = Some t } -> head t
    | t -> t

let rec occur v t = match head t with
    | Ttyp_variable w -> V.equal v w
    | _ -> false

exception UnificationFailure of Typedtree.typ * Typedtree.typ

let unification_error t1 t2 = raise (UnificationFailure (t1, t2))

(* Implements the type unification algorithm.
   See - https://en.wikipedia.org/wiki/Unification_(computer_science)
       - https://www.lri.fr/~filliatr/ens/compil/td/6/index.html *)
let rec unify t1 t2 = match head t1, head t2 with
    | Ttyp_variable v1, Ttyp_variable v2 when V.equal v1 v2 -> ()
    | (Ttyp_variable v1 as t1), t2 ->
        if occur v1 t2 then unification_error t1 t2;
        assert (v1.def = None);
        v1.def <- Some t2
    | t1, (Ttyp_variable v2 as t2) -> unify t2 t1
    | (Ttyp_data (name1, args1), Ttyp_data (name2, args2)) when name1 = name2 ->
        unify_params args1 args2
    | t1, t2 -> unification_error t1 t2 

and unify_params p1 p2 = match (p1, p2) with
    | [], [] -> ()
    | t1::r1, t2::r2 -> unify t1 t2; unify_params r1 r2
    | [], _
    | _, [] -> failwith "not the same number of parameters"

let rec pp_list pp_v fmt = function
    | [] -> ()
    | [x] -> pp_v fmt x
    | x :: xs ->
        Format.fprintf fmt "%a,@ %a" pp_v x (pp_list pp_v) xs

let rec pp_type fmt typ = match typ with
    | Ttyp_variable ({ id }) -> Format.fprintf fmt "t%d" id
    | Ttyp_data (name, []) -> Format.fprintf fmt "%s" name
    | Ttyp_data (name, tvars) -> Format.fprintf fmt "%s @[%a@]" name (pp_list pp_type) tvars
    | _ -> Format.fprintf fmt "unknown"

(* Utility function around unify to emit a proper error in case
   of unification failure with the given source location. *)
let unify_range t1 t2 range =
    try
        unify t1 t2
    with UnificationFailure (t1, t2) ->
        let msg = Format.asprintf "impossible to unify type %a with type %a" pp_type t1 pp_type t2 in
        error range msg

(* $ can not be typed by the user so this identifier is always different from
    any source or user typed identifier. *)
let dummy_ident = { range = Location.dummy_range; node = "$"}

let fill_with_dummy_range node = { range = Location.dummy_range; node }

let type_of_constant c = match c with
    | Ast.Cbool _ -> boolean_type
    | Ast.Cint _ -> int_type
    | Ast.Cstring _ -> string_type
    | Ast.Cunit -> unit_type

let effect_unit = Ttyp_data ("Effect", [unit_type])

let make_node typ range node = { typ; range; node }

let make_sigma tvars args =
    (*tvars = list of variable typ*)
    let new_tvars = Hashtbl.create 17 in
    let ht = Hashtbl.create 17 in
    Hashtbl.iter (
                    fun name (Ttyp_variable(t)) -> 
                        Hashtbl.add new_tvars name (V.create());
                        Hashtbl.add ht t.id name;
                 ) 
                 tvars;
    let rec explore_type current_typ = match current_typ with
        | Ttyp_variable (next_type) -> 
            (
                match next_type.def with 
                    | None -> 
                        let name = Hashtbl.find ht next_type.id in
                        Ttyp_variable(Hashtbl.find new_tvars name)
                    | Some(t) ->
                        let newV = V.create () in
                        newV.def <- Some(explore_type t);
                        Ttyp_variable (newV)
            )
        | Ttyp_function (typ_args, typ_return) ->
            Ttyp_function ((List.map explore_type typ_args), (explore_type typ_return))
        | Ttyp_data (name, args) -> Ttyp_data (name, List.map explore_type args)
    in let new_args = List.map explore_type args in
    (new_tvars, new_args)

(* Checks if the given type has the Eq type class defined for it.
   In other terms, returns true if typ can be compared using == or /=. *)
let is_equalable_type typ =
    let t = head typ in
    t = boolean_type || t = int_type || t = string_type

let rec type_expr genv lenv e = match e.node with
    | Ast.Pexpr_constant c -> make_node (type_of_constant c) e.range (Texpr_constant c)

    | Ast.Pexpr_binary (op, lhs, rhs) ->
        let tlhs = type_expr genv lenv lhs in
        let trhs = type_expr genv lenv rhs in
        let t = match op.node with
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
                if is_equalable_type tlhs.typ then
                    boolean_type
                else
                    (* TODO *)
                    let msg = Format.asprintf "type %a is not comparable for == and /=" pp_type tlhs.typ in
                    error_with_hint tlhs.range msg "expected Boolean, Int or String type"
            | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge ->
                unify_range tlhs.typ int_type tlhs.range;
                unify_range trhs.typ int_type trhs.range;
                boolean_type
            | Ast.Bor | Ast.Band ->
                unify_range tlhs.typ boolean_type tlhs.range;
                unify_range trhs.typ boolean_type trhs.range;
                boolean_type
        in make_node t e.range (Texpr_binary (op, tlhs, trhs))

    | Ast.Pexpr_neg (expr) ->
        (* We transform "-e" to the equivalent code "0 - e". *)
        type_expr genv lenv {
            range = e.range;
            node = Ast.Pexpr_binary (
                { (* The binary operator. As we do not preserve the unary '-'
                     location in the AST, we can not "fake" the location of
                     the binary '-' operator. *)
                    range = Location.dummy_range;
                    node = Ast.Bsub
                },
                { (* The constant 0 does not exist in the source code.
                     Therefore, there is no "source range" for it and so
                     we just pass a dummy invalid range. *)
                    range = Location.dummy_range;
                    node = Pexpr_constant (Cint (0))
                }, expr);
        }

    | Ast.Pexpr_variable name -> (
        match SMap.find_opt name.node lenv with
            | Some typ -> make_node typ e.range (Texpr_variable name)
            | None ->
                (* We handle the variable "unit" differently because it is a
                   special name for a constant of type unit. But unlike true
                   or false, unit is not a keyword and can therefore be redefined
                   by the user. *)
                if name.node <> "unit" then
                    error e.range ("unknown value " ^ name.node)
                else
                    make_node unit_type e.range (Texpr_constant (Cunit))
    )

    | Ast.Pexpr_apply (name, args) -> (
        match Hashtbl.find_opt genv.functions name.node with
        | None -> error name.range ("unknown value " ^ name.node)
        | Some (func_decl) ->
            (* Check the count of arguments. *)
            let args_count = List.length args in
            if args_count <> func_decl.arity then (
                let hint = Printf.sprintf "see %s type declaration at line %d" name.node (fst func_decl.name.range).lineno in
                let msg = Printf.sprintf 
                    "function %s expected %d argument(s), but got %d argument(s)" 
                    name.node 
                    func_decl.arity 
                    args_count 
                in error_with_hint name.range msg hint);

            let targs = List.map (type_expr genv lenv) args in
            let _, func_args_type = make_sigma func_decl.tvars (func_decl.return_type :: func_decl.args_type) in
            (* List.hd func_args_type is the function's return type and
               List.tl func_args_type are the function's arguments type. *)
            (* unify parameters *)
            List.iter2 (fun targ expected_ty -> unify_range targ.typ expected_ty targ.range) targs (List.tl func_args_type);
            let return_type = Ttyp_variable (V.create ()) in
            unify_range (List.hd func_args_type) return_type e.range; (* unify return type *)
            make_node return_type e.range (Texpr_apply (name, targs))
    )

    | Ast.Pexpr_constructor (name, args) -> (
        (* The typing of a constructor call is almost the same as a 
           function apply expression. *)
        match Hashtbl.find_opt genv.constructors name.node with
        | None -> error name.range ("unknown data constructor " ^ name.node)
        | Some (constructor_decl) ->
            let args_count = List.length args in
            if args_count <> constructor_decl.arity then (
                let hint = Printf.sprintf "see %s type declaration at line %d" name.node (fst constructor_decl.name.range).lineno in
                let msg = Printf.sprintf 
                    "constructor %s expected %d argument(s), but got %d argument(s)" 
                    name.node 
                    constructor_decl.arity 
                    args_count
                in error_with_hint name.range msg hint);

            let targs = List.map (type_expr genv lenv) args in
            let data_type = constructor_decl.parent_type in
            let _, constructor_args_type = make_sigma constructor_decl.tvars (data_type :: constructor_decl.args_type) in
            (* List.hd constructor_args_type is the constructor's return type and
               List.tl constructor_args_type are the constructor's arguments type. *)
            (* unify parameters *)
            List.iter2 (fun targ expected_ty -> unify_range targ.typ expected_ty targ.range) targs (List.tl constructor_args_type);
            let return_type = Ttyp_variable (V.create ()) in
            unify_range (List.hd constructor_args_type) return_type e.range; (* unify return type *)
            make_node return_type e.range (Texpr_constructor (name, targs))
    )

    | Ast.Pexpr_if (cond, then_, else_) ->
        let tcond = type_expr genv lenv cond in
        let tthen = type_expr genv lenv then_ in
        let telse = type_expr genv lenv else_ in
        unify_range tcond.typ boolean_type tcond.range;
        unify_range tthen.typ telse.typ tthen.range;
        make_node tthen.typ e.range (Texpr_if (tcond, tthen, telse))

    | Ast.Pexpr_do exprs ->
        let texprs = List.map (fun (e : Ast.expr) ->
            let te = type_expr genv lenv e in
            unify_range te.typ effect_unit te.range;
            te
        ) exprs in
        List.iter (fun t -> unify_range t.typ effect_unit t.range) texprs;
        make_node effect_unit e.range (Texpr_do texprs)

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
                let lenv = (SMap.add name.node tvalue.typ lenv) in
                let texpr = type_expr genv lenv
                    {
                        range = e.range;
                        node = (Ast.Pexpr_let (r, e));
                    }
                in make_node texpr.typ e.range (Texpr_let ((name, tvalue), texpr))
    )

    | Pexpr_case (cond, branches) ->
        let v = Ttyp_variable (V.create ()) in
        let tcond = type_expr genv lenv cond in
        let tbranches = List.map (fun x -> type_branch genv lenv x) branches in
        (* First, check if all branches pattern's type is compatible to tcond's type. *)
        List.iter (fun x -> let p = fst x in unify_range p.typ tcond.typ p.range) tbranches;
        (* Then, check if all branches expressions have the same type. *)
        List.iter (fun x -> let e = snd x in unify_range e.typ v e.range) tbranches;
        make_node v e.range (Texpr_case (tcond, tbranches))

and type_branch genv lenv (pattern, expr) =
    let lenv, tpattern = type_pattern genv lenv pattern in
    let texpr = type_expr genv lenv expr in
    (tpattern, texpr)

and check_arity args expected_arity =
    (List.compare_length_with args expected_arity) = 0

and type_pattern genv lenv pattern = match pattern.node with
    | Ast.Ppattern_constant c ->
        lenv, make_node (type_of_constant c) pattern.range (Tpattern_constant c)

    | Ast.Ppattern_variable name ->
        let typ = Ttyp_variable (V.create ()) in
        SMap.add name.node typ lenv, make_node typ pattern.range (Tpattern_variable (name))

    | Ast.Ppattern_constructor (name, patterns) -> (
        match Hashtbl.find_opt genv.constructors name.node with
        | Some (constructor) ->
            let tpatterns = List.map (type_pattern genv lenv) patterns in
            let lenv = List.fold_left (fun env1 env2 -> 
                SMap.union (fun name _ t -> None) env1 env2) SMap.empty (List.map fst tpatterns) in
            lenv, make_node constructor.parent_type pattern.range (Tpattern_constructor (name, List.map snd tpatterns))

        | None -> error name.range ("unknown data constructor " ^ name.node)
    )

(* Find all the consecutive function equations for the function named fname.
   This also returns all the remaining declarations that are not part of
   the function's equations. *)
let rec collect_equations (fname : string) (decls : decl list) =
    let rec loop equations decls = match decls with
        | [] -> equations, []
        | decl :: r -> (
            match decl.node with
                (* A function equation, just store it and continue the scanning. *)
                | Pdecl_equation (name, _, _) when name.node = fname ->
                    loop (decl :: equations) r

                (* Either not a function with the same name or not a function at all.
                   We found all the function equations. *)
                | _ -> equations, decl :: r
            )
    in loop [] decls

let is_non_variable_pattern = function
    | Ppattern_variable _ -> false
    | _ -> true

(* Check if a function's equation is valid. *)
let type_function_equation genv decl (expected_arity, expected_args_type, expected_ret_type) (name, patterns, body) =
    (* We start to check if the equation's arity is the same as
       the expected arity. *)
    let args_length = List.length patterns in
    if args_length <> expected_arity then (
        let hint = Format.asprintf "got %d, but type declaration requires %d" args_length expected_arity in
        error_with_hint name.range ("incorrect number of arguments for function " ^ name.node) hint
    );

    let lenv = ref SMap.empty in
    (* Then, we check if each pattern is correctly typed. *)
    let tpatterns = List.map2 (fun pattern expected_type -> (
        let _, typed_pattern = type_pattern genv !lenv pattern in
        unify_range typed_pattern.typ expected_type typed_pattern.range;
        (match typed_pattern.node with
            | Tpattern_variable v ->
                lenv := SMap.add v.node typed_pattern.typ !lenv
            | _ -> ());
        typed_pattern
    )) patterns expected_args_type in 
    let tbody = type_expr genv !lenv body in
    unify_range tbody.typ expected_ret_type body.range;
    tpatterns, tbody

    (* TODO:
        (* We also check that there is at most one non-variable pattern
        and it is at the same position as the other non-variables patterns
        in the other equations. *)
        List.iteri (fun i pattern -> (
        if is_non_variable_pattern pattern & i <> non_variable_index then
                raise (Error (pattern.range, "non variable pattern"))
        )) patterns
    *)

let rec from_ast_type genv lenv (typ : Ast.typ) = match typ.node with
    | Ptyp_variable v -> (
        match Hashtbl.find_opt lenv v with
            | Some t -> t
            | None -> error typ.range ("type variable " ^ v ^ " is undefined")
    )

    | Ptyp_data (name, args) ->
        match Hashtbl.find_opt genv.data_types name.node with
            | Some { arity } ->
                let args_count = List.length args in
                if args_count <> arity then (
                    let hint = Format.asprintf "got %d, but %d is required" args_count arity in
                    error_with_hint name.range ("incorrect number of type variables for data " ^ name.node) hint
                );

                Ttyp_data (name.node, List.map (from_ast_type genv lenv) args)

            | None -> error name.range ("unknown type " ^ name.node)

let type_function genv name (decl : Ast.decl) (equations : Ast.decl list) =
    if equations = [] then (
        error decl.range ("type declaration of " ^ name.node ^ " should be followed by its definition")
    );

    let tvars, expected_args_type, expected_ret_type = match decl.node with
        | Pdecl_function (_, quantified_vars, _, args_type, ret_type) ->
            let tvars = Hashtbl.create 17 in
            List.iter (fun v -> (
                Hashtbl.add tvars v.node (Ttyp_variable (V.create ()))
            )) quantified_vars;

            tvars, List.map (from_ast_type genv tvars) args_type, (from_ast_type genv tvars ret_type)
        | _ -> assert false
    in
    let arity = List.length expected_args_type in

    let function_decl = 
    {
        name = name;
        tvars;
        args_type = expected_args_type;
        return_type = expected_ret_type;
        arity = List.length expected_args_type
    } in
    Hashtbl.add genv.functions name.node function_decl;

    (* Check if all function's equations are correctly typed. *)
    let tequations = List.map (fun eq -> match eq.node with
        | Pdecl_equation (name, patterns, body) ->
            snd (type_function_equation genv decl (arity, expected_args_type, expected_ret_type) (name, patterns, body))
        | _ -> failwith "unexpected"
    ) equations in

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
    
    let function_type = Ttyp_function (expected_args_type, expected_ret_type) in
    make_node function_type decl.range (Tdecl_function (name, [], List.hd tequations))

let type_data_constructor genv lenv tvars parent_type (name, args) =
    let targs = List.map (from_ast_type genv lenv) args in

    match Hashtbl.find_opt genv.constructors name.node with
        | None ->
            Hashtbl.add genv.constructors name.node {
                name = name;
                tvars;
                parent_type;
                args_type = targs;
                arity = List.length args;
            };
            (name, targs)

        | Some previous_decl ->
            let previous_decl_line = (fst previous_decl.name.range).lineno in
            let hint = Format.sprintf "data constructor %s was declared before at line %d" name.node previous_decl_line in
            error_with_hint name.range ("redeclaration of data constructor " ^ name.node) hint

let type_data genv name range tvars constructors =
    let lenv = Hashtbl.create 17 in
    let tvars_type = ref [] in
    List.iter (fun tvar ->
        match Hashtbl.find_opt lenv tvar.node with
        | None ->
            let t = Ttyp_variable (V.create ()) in
            Hashtbl.add lenv tvar.node t;
            tvars_type := t :: !tvars_type

        | Some _ ->
            error tvar.range ("type argument " ^ tvar.node ^ " appears more than once")
    ) tvars;

    let data_type = Ttyp_data (name.node, !tvars_type) in
    Hashtbl.add genv.data_types name.node { name; data_type; arity = List.length tvars };
    
    let tconstructors = List.map (type_data_constructor genv lenv lenv data_type) constructors in
    make_node data_type range (Tdecl_data (name, tconstructors))

let mk_builtin_function name args_type return_type =
    {
        name;
        tvars = Hashtbl.create 0;
        args_type;
        return_type;
        arity = List.length args_type;
    }

let default_genv =
    let functions = Hashtbl.create 17 in
    let constructors = Hashtbl.create 17 in
    let data_types = Hashtbl.create 17 in
    let class_types = Hashtbl.create 17 in

    Hashtbl.add data_types "Unit" { name = fill_with_dummy_range "Unit"; data_type = unit_type; arity = 0 };
    Hashtbl.add data_types "Boolean" { name = fill_with_dummy_range "Boolean"; data_type = boolean_type; arity = 0 };
    Hashtbl.add data_types "Int" { name = fill_with_dummy_range "Int"; data_type = int_type; arity = 0 };
    Hashtbl.add data_types "String" { name = fill_with_dummy_range "String"; data_type = string_type; arity = 0 };
    Hashtbl.add data_types "Effect" { name = fill_with_dummy_range "Effect"; data_type = effect_unit; arity = 1 };

    (* the constant unit is handled separately in type_expr *)
    Hashtbl.add functions "not" (mk_builtin_function (fill_with_dummy_range "not") [boolean_type] boolean_type);
    Hashtbl.add functions "mod" (mk_builtin_function (fill_with_dummy_range "mod") [int_type; int_type] int_type);
    Hashtbl.add functions "log" (mk_builtin_function (fill_with_dummy_range "log") [string_type] effect_unit);

    let a = Ttyp_variable (V.create ()) in
    let tvars = Hashtbl.create 1 in Hashtbl.add tvars "a" a;
    Hashtbl.add functions "pure" {
        name = fill_with_dummy_range "pure";
        tvars;
        args_type = [a];
        return_type = (Ttyp_data ("Effect", [a]));
        arity = 1;
    };

    {
        functions;
        constructors;
        data_types;
        class_types;
    }

let file decls = 
    let genv = default_genv in
    (* Collect all declarations: *)
    let rec loop acc decls = match decls with
        | [] -> acc
        | decl :: r ->
            match decl.node with
                (* Handle functions *)
                | Pdecl_equation (name, _, _) ->
                    error name.range ("value " ^ name.node ^ " has no type declaration")

                | Pdecl_function (name, _, _, _, _) -> (
                    match Hashtbl.find_opt genv.functions name.node with
                        | None ->
                            let equations, next_decls = collect_equations name.node r in
                            let tdecl = type_function genv name decl equations in
                            loop (tdecl :: acc) next_decls
                        | Some previous_decl ->
                            let previous_decl_line = (fst previous_decl.name.range).lineno in
                            let hint = Format.sprintf "previous declaration of value %s is at line %d" name.node previous_decl_line in
                            error_with_hint name.range ("redeclaration of value " ^ name.node) hint
                )
                (* Handle data types and constructors *)
                | Pdecl_data (name, tvars, constructors) -> (
                    match Hashtbl.find_opt genv.data_types name.node with
                        | None -> loop ((type_data genv name decl.range tvars constructors) :: acc) r
                        
                        | Some previous_decl ->
                            let previous_decl_line = (fst previous_decl.name.range).lineno in
                            let hint = Format.sprintf "previous declaration of data type %s is at line %d" name.node previous_decl_line in
                            error_with_hint name.range ("redeclaration of data type " ^ name.node) hint
                )

                | Pdecl_class (name, _, _) -> failwith "class declarations not yet implemented"
                | Pdecl_instance (_, _) -> failwith "instance declarations not yet implemented"
    in let tdecls = loop [] decls in

    if Option.is_none (Hashtbl.find_opt genv.functions "main") then
        error Location.dummy_range "value main is missing"
    else
        tdecls
