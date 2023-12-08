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
    | Ttyp_variable { def = Some t } -> head t
    | t -> t

let rec occur v t = match head t with
    | Ttyp_variable w -> V.equal v w
    | _ -> false
    exception UnificationFailure of Typedtree.typ * Typedtree.typ
    let unification_error t1 t2 = raise (UnificationFailure (t1, t2))
    
let rec unify t1 t2 = match head t1, head t2 with
    | Ttyp_int, Ttyp_int 
    | Ttyp_boolean, Ttyp_boolean 
    | Ttyp_string, Ttyp_string
    | Ttyp_unit, Ttyp_unit -> ()
    | Ttyp_variable v1, Ttyp_variable v2 when V.equal v1 v2 -> ()
    | (Ttyp_variable v1 as t1), t2 -> 
        if occur v1 t2 then unification_error t1 t2;
        assert (v1.def = None);
        v1.def <- Some t2
    | t1, (Ttyp_variable v2 as t2) -> unify t2 t1
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
            let v = Ttyp_variable (V.create ()) in
            unify decl.typ (Ttyp_function (args_type, v));
            {
                typ = v;
                node = Texpr_apply (name, typed_args)
            }
    )

    | _ -> raise (Error (e.range, "not yet implemented"))

(* Find the declaration and all equations corresponding to the given 
   function name as a couple decl * decl list. 
   
   If no declaration is found for the function or if there is multiple
   declarations for the same function, then an error is emitted at
   the location either of the last declaration or the first function
   equation. *)
let rec find_function (fname : string) (decls : decl list) =
    let range_to_use = ref (Location.dummy, Location.dummy) in
    let rec loop function_decl equations decls = match decls with
        | [] -> function_decl, equations
        | decl :: r -> (
            match decl.node with
                (* A function equation, just store it and continue the scanning. *)
                | Pdecl_equation (name, _, _) when name.node = fname -> (
                    range_to_use := decl.range;
                    loop function_decl (decl :: equations) r
                )

                (* A function declaration. We need to check if there is no multiple
                   declarations for the same function as it is an error. *)
                | Pdecl_function (name, _, _, _, _) when name.node = fname -> (
                    if Option.is_some function_decl then (
                        (* The function has multiple declarations which is not allowed. *)
                        raise (Error (decl.range, "function '" ^ fname ^ "' declared multiple times"))
                    ) else (
                        loop (Some decl) equations r
                    )
                )

                (* Either not a function with the same name or not a function at all.
                   We just ignore it and continue our scanning. *)
                | _ -> loop function_decl equations r
            )
    in
    let decl, equations = loop None [] decls in
    
    (* Check if the function has a at least one declaration specified. *)
    if Option.is_none decl then (
        raise (Error (!range_to_use, "no function declaration for '" ^ fname ^ "'"))
    ) else (
        Option.get decl, equations
    )

let check_function_equation decl expected_arity (name, args, body) =
    (* We start to check if the equation's arity is the same
       as the function declaration arity. *)
    if (List.compare_length_with args expected_arity) = 0 then (
        raise (Error (decl.range, "arity mismatch"))
    );

    List.map2

let file decls =
    let global_env = Hashtbl.create 17 in
    ()
