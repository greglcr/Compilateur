open Typedtree
open Ast

exception Error of (Location.t * Location.t) * string

module SMap = Map.Make(String)
module SSet = Set.Make(String)

let dummy_range = Location.dummy, Location.dummy

(* Some utilities functions to check if an identifier is a LIDENT or a UIDENT. *)
let is_lowercase c = (Char.lowercase_ascii c) = c
let is_uppercase c = (Char.uppercase_ascii c) = c
let is_lident name = is_lowercase (String.get name.node 0)
let is_uident name = is_uppercase (String.get name.node 0)

type global_env =
    {
        functions : (string, function_decl) Hashtbl.t;
        constructors : (string, constructor_decl) Hashtbl.t;
        data_types : (string, Typedtree.typ) Hashtbl.t;
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
    | (Ttyp_data (name1, args1), Ttyp_data (name2, args2)) when name1 = name2 ->
        unify_params args1 args2
    | t1, t2 -> unification_error t1 t2 

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

(* $ can not be typed by the user so this identifier is always different from
    any source or user typed identifier. *)
let dummy_ident = fill_with_dummy_range "$"

let type_of_constant c = match c with
    | Ast.Cbool _ -> Ttyp_boolean
    | Ast.Cint _ -> Ttyp_int
    | Ast.Cstring _ -> Ttyp_string
    | Ast.Cunit -> Ttyp_unit

let effect_unit = Ttyp_effect (Ttyp_unit)

let make_node typ node = { typ; node }

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
        | Ttyp_unit
        | Ttyp_boolean
        | Ttyp_int
        | Ttyp_string -> current_typ
        | Ttyp_effect (next_type) -> Ttyp_effect(explore_type next_type)
        | Ttyp_variable (next_type) -> 
            (
                match next_type.def with 
                    | None -> 
                        let name = Hashtbl.find ht next_type.id in
                        Printf.printf "First find";
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

let is_equalable_type typ =
    let t = head typ in
    t = Ttyp_boolean || t = Ttyp_int || t = Ttyp_string

let rec type_expr genv lenv e = match e.node with
    | Ast.Pexpr_constant c -> make_node (type_of_constant c) (Texpr_constant c)

    | Ast.Pexpr_binary (op, lhs, rhs) ->
        let tlhs = type_expr genv lenv lhs in
        let trhs = type_expr genv lenv rhs in
        let t = match op.node with
            | Ast.Badd | Ast.Bsub | Ast.Bmul | Ast.Bdiv ->
                unify tlhs.typ Ttyp_int;
                unify trhs.typ Ttyp_int;
                Ttyp_int
            | Ast.Bconcat ->
                unify tlhs.typ Ttyp_string;
                unify trhs.typ Ttyp_string;
                Ttyp_string
            | Ast.Beq | Ast.Bneq ->
                unify tlhs.typ trhs.typ;
                if is_equalable_type tlhs.typ then
                    Ttyp_boolean
                else
                    raise (Error (lhs.range, "expected Boolean, Int or String type"))
            | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge ->
                unify tlhs.typ Ttyp_int;
                unify trhs.typ Ttyp_int;
                Ttyp_boolean
            | Ast.Bor | Ast.Band ->
                unify tlhs.typ Ttyp_boolean;
                unify trhs.typ Ttyp_boolean;
                Ttyp_boolean
        in make_node t (Texpr_binary (op, tlhs, trhs))

    | Ast.Pexpr_neg (expr) ->
        (* We transform "-e" to the equivalent code "0 - e". *)
        type_expr genv lenv {
            range = e.range;
            node = Ast.Pexpr_binary (
                { (* The binary operator. As we do not preserve the unary '-'
                     location in the AST, we can not "fake" the location of
                     the binary '-' operator. *)
                    range = dummy_range;
                    node = Ast.Bsub
                },
                { (* The constant 0 does not exist in the source code.
                     Therefore, there is no "source range" for it and so
                     we just pass a dummy invalid range. *)
                    range = dummy_range;
                    node = Pexpr_constant (Cint (0))
                }, expr);
        }

    | Ast.Pexpr_variable name -> (
        match SMap.find_opt name.node lenv with
            | Some typ -> make_node typ (Texpr_variable name)
            | None ->
                (* We handle the variable "unit" differently because it is a
                   special name for a constant of type unit. But unlike true
                   or false, unit is not a keyword and can therefore be redefined
                   by the user. *)
                if name.node <> "unit" then
                    raise (Error (e.range, "unknown variable '" ^ name.node ^ "'"))
                else
                    make_node Ttyp_unit (Texpr_constant (Cunit))
    )

    | Ast.Pexpr_apply (name, args) -> (
        match Hashtbl.find_opt genv.functions name.node with
        | None -> raise (Error (name.range, "function '" ^ name.node ^ "' not found"))
        | Some (func_decl) ->
            let targs = List.map (type_expr genv lenv) args in
            let args_type = List.map (fun a -> a.typ) targs in
            let _, func_args_type = make_sigma func_decl.tvars (func_decl.return_type :: func_decl.args_type) in
            (* List.hd func_args_type is the function's return type and
               List.tl func_args_type are the real arguments type. *)
            unify_params (List.tl func_args_type) args_type; (* unify parameters *)
            let v = Ttyp_variable (V.create ()) in
            unify (List.hd func_args_type) v; (* unify return type *)
            make_node v (Texpr_apply (name, targs))
    )

    | Ast.Pexpr_constructor (name, args) -> (
        match Hashtbl.find_opt genv.constructors name.node with
        | None -> raise (Error (name.range, "constructor '" ^ name.node ^ "' not found"))
        | Some (constructor_decl) ->
            let targs = List.map (type_expr genv lenv) args in
            let args_type = List.map (fun a -> a.typ) targs in
            let data_type = constructor_decl.data_type in
            let _, constructor_args_type = make_sigma constructor_decl.tvars (data_type :: constructor_decl.args_type) in
            unify_params (List.tl constructor_args_type) args_type; (* unify parameters *)
            let v = Ttyp_variable (V.create ()) in
            unify (List.hd constructor_args_type) v; (* unify return type *)
            make_node v (Texpr_constructor (name, targs))
    )

    | Ast.Pexpr_if (cond, then_, else_) ->
        let tcond = type_expr genv lenv cond in
        let tthen = type_expr genv lenv then_ in
        let telse = type_expr genv lenv else_ in
        unify tcond.typ Ttyp_boolean;
        unify tthen.typ telse.typ;
        make_node tthen.typ (Texpr_if (tcond, tthen, telse))

    | Ast.Pexpr_do exprs ->
        let texprs = List.map (fun (e : Ast.expr) ->
            let te = type_expr genv lenv e in
            unify te.typ effect_unit;
            te
        ) exprs in
        List.iter (fun t -> unify t.typ effect_unit) texprs;
        make_node effect_unit (Texpr_do texprs)

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
                in make_node texpr.typ (Texpr_let ((name, tvalue), texpr))
    )

    | Pexpr_case (cond, branches) ->
        let v = Ttyp_variable (V.create ()) in
        let tcond = type_expr genv lenv cond in
        let tbranches = List.map (fun x -> type_branch genv lenv x) branches in
        List.iter (fun x -> unify (fst x).typ tcond.typ) tbranches;
        List.iter (fun x -> unify (snd x).typ v) tbranches;
        make_node v (Texpr_case (tcond, tbranches))

and type_branch genv lenv (pattern, expr) =
    let lenv, tpattern = type_pattern genv lenv pattern in
    let texpr = type_expr genv lenv expr in
    (tpattern, texpr)
    
    

and check_arity args expected_arity =
    (List.compare_length_with args expected_arity) = 0

and type_pattern genv lenv pattern = match pattern.node with
    | Ast.Ppattern_constant c ->
        lenv, make_node (type_of_constant c) (Tpattern_constant c)

    | Ast.Ppattern_variable name ->
        let typ = Ttyp_variable (V.create ()) in
        SMap.add name.node typ lenv, make_node typ (Tpattern_variable (name))

    | Ast.Ppattern_constructor (name, patterns) -> (
        match Hashtbl.find_opt genv.constructors name.node with
        | Some (constructor) ->
            let tpatterns = List.map (type_pattern genv lenv) patterns in
            let lenv = List.fold_left (fun env1 env2 -> 
                SMap.union (fun name _ t -> None) env1 env2) SMap.empty (List.map fst tpatterns) in
            lenv, make_node constructor.data_type (Tpattern_constructor (name, List.map snd tpatterns))

        | None -> raise (Error (name.range, "unknown constructor " ^ name.node))
    )

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
        (Option.get decl), equations
    )

let is_non_variable_pattern = function
    | Ppattern_variable _ -> false
    | _ -> true

(* Check if a function's equation is valid. *)
let type_function_equation genv decl (expected_arity, expected_args_type, expected_ret_type) (name, patterns, body) =
    Printf.printf "TYPING FUNCTION EQUATION %s\n" name.node;
    (* We start to check if the equation's arity is the same as
       the expected arity. *)
    Printf.printf "%d %d\n" (List.length patterns) expected_arity;
    if (List.compare_length_with patterns expected_arity) <> 0 then (
        raise (Error (decl.range, "arity mismatch"))
    );

    let lenv = ref SMap.empty in
    (* Then, we check if each pattern is correctly typed. *)
    let typed_patterns = List.map2 (fun pattern expected_type -> (
        let _, typed_pattern = type_pattern genv !lenv pattern in
        unify typed_pattern.typ expected_type;
        (match typed_pattern.node with
            | Tpattern_variable v ->
                lenv := SMap.add v.node typed_pattern.typ !lenv
            | _ -> ());
        typed_pattern
    )) patterns expected_args_type in 
    let tbody = type_expr genv !lenv body in
    unify tbody.typ expected_ret_type;
    tbody

    (* TODO:
        (* We also check that there is at most one non-variable pattern
        and it is at the same position as the other non-variables patterns
        in the other equations. *)
        List.iteri (fun i pattern -> (
        if is_non_variable_pattern pattern & i <> non_variable_index then
                raise (Error (pattern.range, "non variable pattern"))
        )) patterns
    *)

let from_ast_type genv lenv (typ : Ast.typ) = match typ.node with
    | Ptyp_variable v -> (
        match Hashtbl.find_opt lenv v with
            | Some t -> t
            | None -> raise (Error (typ.range, "unknown type variable '" ^ v ^ "'"))
    )

    | Ptyp_data ({ node = "Boolean" }, []) -> Ttyp_boolean
    | Ptyp_data ({ node = "Int" }, []) -> Ttyp_int
    | Ptyp_data ({ node = "String" }, []) -> Ttyp_string

    | Ptyp_data (name, args) ->
        match Hashtbl.find_opt genv.data_types name.node with
            | Some data_type -> data_type
            | None -> raise (Error (name.range, "unknown constructor '" ^ name.node ^ "'"))

let type_function genv name (decl : Ast.decl) (equations : Ast.decl list) =
    Printf.printf "TYPING FUNCTION %s (with %d equations)" name.node (List.length equations);
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
        name = name.node;
        tvars;
        args_type = expected_args_type;
        return_type = expected_ret_type;
        arity = List.length expected_args_type
    } in
    Hashtbl.add genv.functions name.node function_decl;

    (* Check if all function's equations are correctly typed. *)
    let typed_equations = List.map (fun eq -> match eq.node with
        | Pdecl_equation (name, patterns, body) ->
            type_function_equation genv decl (arity, expected_args_type, expected_ret_type) (name, patterns, body)
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
    make_node function_type (Tdecl_function (name, [], List.hd typed_equations))

(*
let find_in_env genv locenv key =
    try Hashtbl.find genv key
    with Not_found -> try Hashtbl.find locenv key
                      with Not_found -> raise (Error (dummy_range, "type not found"))
(*Add Ttyp data in Ast?*)
let type_data genv lenv decl = match decl with
    | Pdecl_data (name, args, fields) -> (
        let loc_env = Hashtbl.create 17 in
        let list_typ = ref [] in
        List.iter (fun x -> let new_typ = Ttyp_variable (V.create()) in list_typ := new_typ :: (!list_typ); Hashtbl.add loc_env x args)
        Hashtbl.add genv.items name (Tdecl_data (name, !list_typ));
        (*We go through the list of fields in order to declare function that have the argument given and a return type of the data*)
        let rec add_fields l = match l withe
    | _ -> assert false⟨lident⟩ :: (forall ⟨lident⟩+.)?
(⟨ntype⟩ =>)⋆ (⟨type⟩ ->)⋆ ⟨type⟩
                fun x -> 
                if Hashtbl.mem loc_env x || Hashtbl.mem genv x then raise (Error (dummy_range, "Redclaration of type"))
                else Hashtbl.add lov_env x (V.create ()) (*Maybe we need to check if the type doesn't already exist, and if it the case raise a typing error*)
            )
            agrs;
        List.iter
            (
                fun x ->
                type_function genv loc_env x
            )
            decls;
    | _ -> assert false
*)

(*When we define a calss C a1 .. an, we can use it in a instance with instance C Int.abs
   An instance is either a ntype, either a ntype => ntype, either 
   We can also have fatarrow in function declaration
   When declaring an instance, when can add a list of nype like instance name ( ntype)
   In purescript, instance can have names but it's never possible in minipurescript
   We must also check that all general types declared are used.
   Should we check that an instance declaration 
   *)

(*let type_instances decl_inst genv loc_env = match inst with
    | Pdecl_instance (target, decls) -> let 
    | _ -> assert false*)


let type_data_constructor genv lenv tvars data_type (name, args) =
    let targs = List.map (from_ast_type genv lenv) args in

    Hashtbl.add genv.constructors name.node {
        name = name.node;
        tvars;
        data_type;
        args_type = targs;
        arity = List.length args;
     };

    (name, targs)
     

let type_data genv name tvars constructors =
    let lenv = Hashtbl.create 17 in
    let tvars_type = ref [] in
    List.iter (fun tvar -> 
        let t = Ttyp_variable (V.create ()) in
        Hashtbl.add lenv tvar.node t;
        tvars_type := t :: !tvars_type
    ) tvars;

    let data_type = Ttyp_data (name.node, !tvars_type) in
    Hashtbl.add genv.data_types name.node data_type;
    
    let tconstructors = List.map (type_data_constructor genv lenv lenv data_type) constructors in
    make_node data_type (Tdecl_data (name, tconstructors))

let file decls = 
    let genv = { 
        functions = Hashtbl.create 17;
        constructors = Hashtbl.create 17;
        data_types = Hashtbl.create 17;
        class_types = Hashtbl.create 17;
    } in
    (* Collect all declarations: *)
    List.fold_left (fun acc decl -> (
        match decl.node with
            (* Handle functions *)
            | Pdecl_equation (name, _, _)
            | Pdecl_function (name, _, _, _, _) -> (
                match Hashtbl.find_opt genv.functions name.node with
                    | None ->
                        let decl, equations = find_function name.node decls in
                        let tdecl = type_function genv name decl equations in
                        tdecl :: acc
                    | Some _ ->
                        (* Ignore it as this declaration was already handled by find_function
                        at the first time this function's name was encountered. *)
                        acc
            )

            (* Handle data types and constructors *)
            | Pdecl_data (name, tvars, constructors) -> (
                match Hashtbl.find_opt genv.data_types name.node with
                    | None -> (type_data genv name tvars constructors) :: acc
                    
                    | Some (previous_decl) ->
                        (* TODO: print the location of the previous declaration. *)
                        raise (Error (name.range, "redeclaration of data type " ^ name.node))
            )

            | Pdecl_class (name, _, _) -> failwith "class declarations not yet implemented"
            | Pdecl_instance (_, _) -> failwith "instance declarations not yet implemented"
    )) [] decls
