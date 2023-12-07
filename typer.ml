open Typedtree
open Ast

exception Error of (Location.t * Location.t) * string

module SMap = Map.Make(String)
module SSet = Set.Make(String)

(*
There many simplications over the real PureScript about the typing:
  - There no type inference for functions signature. That is, a function
    declaration is always given along a function signature (eg. f :: T1 -> T2 -> T3).
  - Case expression with multiple patterns are not allowed:
        case e1, e2 of ...
    Therefore, functions using multiple patterns as arguments are not allowed.
*)
module V = struct
    type t = tvar
    let compare v1 v2 = Stdlib.compare v1.id v2.id
    let equal v1 v2 = v1.id = v2.id
    let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end

let rec head = function
    | Ttyp_var { def = Some t } -> head t
    | t -> t

let rec occur v t = match head t with
    | Ttyp_var w -> V.equal v w
    | _ -> false
    exception UnificationFailure of Typedtree.typ * Typedtree.typ
    let unification_error t1 t2 = raise (UnificationFailure (t1, t2))
    
let rec unify t1 t2 = match head t1, head t2 with
    | Ttyp_int, Ttyp_int 
    | Ttyp_boolean, Ttyp_boolean 
    | Ttyp_string, Ttyp_string
    | Ttyp_unit, Ttyp_unit -> ()
    | Ttyp_var v1, Ttyp_var v2 when V.equal v1 v2 -> ()
    | (Ttyp_var v1 as t1), t2 -> 
        if occur v1 t2 then unification_error t1 t2;
        assert (v1.def = None);
        v1.def <- Some t2
    | t1, (Ttyp_var v2 as t2) -> unify t2 t1
    | Ttyp_function (pl1, e1), Ttyp_function (pl2, e2) -> 
        unify_params pl1 pl2;
        unify e1 e2
    | _ -> ()

and unify_params p1 p2 = match (p1, p2) with
    | [], [] -> ()
    | t1::r1, t2::r2 -> unify t1 t2; unify_params r1 r2
    | [], _ 
    | _, [] -> raise (Error ((Location.dummy, Location.dummy), "not the same number of parameters"))

type env =
    {
        decls : (string, Ast.decl) Hashtbl.t
    }

let fill_with_dummy_range node =
    {
        range = (Location.dummy, Location.dummy);
        node
    }

(* A dummy identifier used in generated code. *)
let dummy_ident =
    (* $ can not be typed by the user so this identifier is always different from
        any source or user typed identifier. *)
    fill_with_dummy_range "$"

let effect_unit = Ttyp_effect (Ttyp_unit)
let check_if_unit expr =
    if expr.typ <> effect_unit then
        raise (Error ((Location.dummy, Location.dummy), "expected Effect Unit type"))

let check_if_int expr =
    if expr.typ <> Ttyp_int then
        raise (Error ((Location.dummy, Location.dummy), "expected Int type"))

let check_if_string expr =
    if expr.typ <> Ttyp_string then
        raise (Error ((Location.dummy, Location.dummy), "expected String type"))

let check_if_boolean expr =
    if expr.typ <> Ttyp_boolean then
        raise (Error ((Location.dummy, Location.dummy), "expected Boolean type"))

let check_if_has_eq expr =
    if (expr.typ <> Ttyp_boolean) || (expr.typ <> Ttyp_int) || (expr.typ <> Ttyp_string) then
        raise (Error ((Location.dummy, Location.dummy), "expected Boolean, Int or String type"))

let rec expr global_env state e = match e.node with
    | Ast.Pexpr_constant c -> (
        let t = match c with
            | Cbool _ -> Ttyp_boolean
            | Cint _ -> Ttyp_int
            | Cstring _ -> Ttyp_string
        in
        {
            typ = t;
            node = Texpr_constant c;
        }
    )

    | Ast.Pexpr_binary (op, lhs, rhs) -> (
        let tlhs = expr global_env state lhs in
        let trhs = expr global_env state rhs in

        let t = match op.node with
            | Ast.Badd | Ast.Bsub | Ast.Bmul | Ast.Bdiv -> (
                unify tlhs.typ Ttyp_int;
                unify trhs.typ Ttyp_int;
                Ttyp_int
            )
            | Ast.Bconcat -> (
                unify tlhs.typ Ttyp_string;
                unify trhs.typ Ttyp_string;
                Ttyp_string
            )
            | Ast.Beq | Ast.Bneq -> (
                check_if_has_eq tlhs;
                check_if_has_eq trhs;
                Ttyp_boolean
            )
            | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge -> (
                unify tlhs.typ Ttyp_int;
                unify trhs.typ Ttyp_int;
                Ttyp_boolean
            )
            | Ast.Bor | Ast.Band -> (
                unify tlhs.typ Ttyp_boolean;
                unify trhs.typ Ttyp_boolean;
                Ttyp_boolean
            )
        in

        {
            typ = t;
            node = Texpr_binary (op.node, tlhs, trhs);
        }
    )

    | Ast.Pexpr_if (cond, then_, else_) -> (
        let tcond = expr global_env state cond in
        let tthen = expr global_env state then_ in
        let telse = expr global_env state else_ in

        unify tcond.typ Ttyp_boolean;

        if tthen <> telse then (
            raise (Error (else_.range, "else branch must have the same type as the then branch"))
        );

        {
            typ = tthen.typ;
            node = Texpr_if (tcond, tthen, telse);
        }
    )

    | Ast.Pexpr_do exprs -> (
        let texprs = List.map (fun (e : Ast.expr) -> expr global_env state e) exprs in
        List.iter (fun t -> unify t.typ effect_unit) texprs;

        {
            typ = Ttyp_unit;
            node = Texpr_do texprs;
        }
    )

    | Ast.Pexpr_let (bindings, e) -> (
        match bindings with
            | [] -> expr global_env state e
            | (name, value) :: r -> 
                let typed_binding = expr global_env state value in
                let typed_expr = expr global_env
                    (SMap.add name.node typed_binding.typ state)
                    {
                        range = e.range;
                        node = (Ast.Pexpr_let (r, e));
                    }
                in
                {
                    typ = typed_expr.typ;
                    node = Texpr_let ((name, typed_binding), typed_expr);
                }
    )

    | Ast.Pexpr_apply (name, args) -> (
        match Hashtbl.find_opt global_env name.node with
        | None -> raise (Error (name.range, "function '" ^ name.node ^ "' not found"))
        | Some (decl) ->
            let typed_args = List.map (fun a -> expr global_env state a) args in
            let args_type = List.map (fun a -> a.typ) typed_args in
            let v = Ttyp_var (V.create ()) in
            unify decl.typ (Ttyp_function (args_type, v));
            {
                typ = v;
                node = Texpr_apply (name, typed_args)
            }
    )

    | _ -> raise (Error (e.range, "not yet implemented"))

let rec find_functions functions decls =
    let handle_decl decl = match decl with
        | Pdecl_func (name, patterns, expr) -> (
            Hashtbl.add functions name.node decl
        )
        | Pdecl_func_signature (name, _, _, _, _) -> (
        Hashtbl.add functions name.node decl
        )
        | _ -> ()
    in match decls with
    | [] -> functions
    | decl::remaining -> (handle_decl decl.node); find_functions  functions remaining

let compress_function functions name =
    let all_declarations = Hashtbl.find_all functions name in

    (*
        We want to "compress" function declarations. That is, we want to merge
        multiple declarations of the same function into a single one. For that,
        we apply the following transformation :
        If we have the following declarations (f the function name; p1, p2...
        patterns; e1, e2... expressions) :

            f p1 = e1
            f p2 = e2
            ...
            f pn = en

        Then we "compress" it to the single declaration :

            f x =
                case x of
                    p1 -> e1
                    p2 -> e2
                    ...
                    pn -> en

        However, if there is one declaration with more than one pattern (more
        than a single argument), then we fail (this is because we do not support
        `case' with multiple patterns).
        So, for example :

            f a b = e1
            f c d = e2

        with a, b, c and d patterns; will fail.
        One exception is if all patterns are only variables. As this does not
        require pattern matching (it is just a classical function with arguments).

        Finally, if there are two declarations with a different count of patterns,
        then an error is emitted.
    *)

    let is_variable_pattern pattern = match pattern.node with
        | Ppattern_variable _ -> true
        | _ -> false
    in

    (* Accumulated patterns of all declarations *)
    let signature = ref None in
    let function_fully_declared = ref false in
    let has_simple_declaration = ref None in
    let last_declaration_name_range = ref None in
    
    let patterns = List.fold_left (fun previous_patterns decl -> match decl with
        | Pdecl_func ({ range }, patterns, expr) -> (
            if !function_fully_declared then (
                let range = Option.get !last_declaration_name_range in
                raise (Error (range, "function '" ^ name ^ "' already declared"))
            ) else (
                last_declaration_name_range := Some range;
            );

            let all_patterns_are_variables = List.for_all is_variable_pattern patterns in
            let patterns_length = List.length patterns in
            if all_patterns_are_variables || patterns_length == 0 then (
                function_fully_declared := true;
                has_simple_declaration := Some decl;
                []
            ) else if patterns_length == 1 then (
                has_simple_declaration := None;
                (List.hd patterns, expr) :: previous_patterns
            ) else (
                raise (Error (range, "too many patterns in '" ^ name ^ "' function declaration for "))
            )
        )

        | Pdecl_func_signature ({ range }, _, _, _, _) -> (
            if Option.is_some !signature then (
                raise (Error (range, "function signature specified more than once for '" ^ name ^ "'"))
            );

            signature := Some decl;
            []
        )

        | _ -> assert false
    ) [] all_declarations
    in

    if Option.is_none !signature then (
        raise (Error (Option.get !last_declaration_name_range, "function " ^ name ^ " has no signature"))
    );

    if Option.is_none !has_simple_declaration then (
        let case_expr = Pexpr_case (
            fill_with_dummy_range (Pexpr_apply (dummy_ident, [])),
            patterns
        ) in
        (Option.get !signature), (Pdecl_func (fill_with_dummy_range name, [ fill_with_dummy_range (Ppattern_variable "$") ], fill_with_dummy_range case_expr))
    ) else (
        (Option.get !signature), (Option.get !has_simple_declaration)
    )

let file decls =
    let global_env = Hashtbl.create 17 in
    let functions = find_functions (Hashtbl.create 31) decls in
    let functions_name = List.of_seq (SSet.to_seq (SSet.of_seq (Hashtbl.to_seq_keys functions))) in
    let compressed_functions = List.map (fun name -> compress_function functions name) functions_name in
    List.iter (fun (signature, f) -> (
        match f with
        | Pdecl_func (name, params, e) -> (
            let typed_body = expr global_env SMap.empty e in
            Hashtbl.add global_env name.node 
                {
                    typ = Ttyp_function ([], effect_unit);
                    node = (Tdecl_function (name, [], typed_body));
                };
        )
        | _ -> assert false
    )) compressed_functions
