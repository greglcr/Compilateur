open Typedtree
open Ast

exception Error of (Location.t * Location.t) * string

module SMap = Map.Make(String)
module SSet = Set.Make(String)

let dummy_range = Location.dummy, Location.dummy

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

let type_of_constant c = match c with
    | Ast.Cbool _ -> Ttyp_boolean
    | Ast.Cint _ -> Ttyp_int
    | Ast.Cstring _ -> Ttyp_string

let effect_unit = Ttyp_effect (Ttyp_unit)

let check_if_has_eq expr =
    if (expr.typ <> Ttyp_boolean) || (expr.typ <> Ttyp_int) || (expr.typ <> Ttyp_string) then
        raise (Error ((Location.dummy, Location.dummy), "expected Boolean, Int or String type"))

let rec type_expr genv lenv e = match e.node with
    | Ast.Pexpr_constant c ->
        {
            typ = type_of_constant c;
            node = Texpr_constant c;
        }

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
                check_if_has_eq tlhs;
                check_if_has_eq trhs;
                Ttyp_boolean
            | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge ->
                unify tlhs.typ Ttyp_int;
                unify trhs.typ Ttyp_int;
                Ttyp_boolean
            | Ast.Bor | Ast.Band ->
                unify tlhs.typ Ttyp_boolean;
                unify trhs.typ Ttyp_boolean;
                Ttyp_boolean
        in
        {
            typ = t;
            node = Texpr_binary (op.node, tlhs, trhs);
        }

    | Ast.Pexpr_if (cond, then_, else_) ->
        let tcond = type_expr genv lenv cond in
        let tthen = type_expr genv lenv then_ in
        let telse = type_expr genv lenv else_ in

        unify tcond.typ Ttyp_boolean;

        if tthen <> telse then (
            raise (Error (else_.range, "else branch must have the same type as the then branch"))
        );

        {
            typ = tthen.typ;
            node = Texpr_if (tcond, tthen, telse);
        }

    | Ast.Pexpr_do exprs ->
        let texprs = List.map (fun (e : Ast.expr) -> type_expr genv lenv e) exprs in
        List.iter (fun t -> unify t.typ effect_unit) texprs;
        {
            typ = Ttyp_unit;
            node = Texpr_do texprs;
        }

    | Ast.Pexpr_let (bindings, e) -> 
        match bindings with
            | [] -> type_expr genv lenv e
            | (name, value) :: r -> 
                let tbinding = type_expr genv lenv value in
                let texpr = type_expr genv
                    (SMap.add name.node tbinding.typ lenv)
                    {
                        range = e.range;
                        node = (Ast.Pexpr_let (r, e));
                    }
                in
                {
                    typ = texpr.typ;
                    node = Texpr_let ((name, tbinding), texpr);
                }

    | Ast.Pexpr_apply (name, args) ->
        match Hashtbl.find_opt genv.items name.node with
        | None -> raise (Error (name.range, "function '" ^ name.node ^ "' not found"))
        | Some (decl) ->
            let targs = List.map (fun a -> type_expr genv lenv a) args in
            let args_type = List.map (fun a -> a.typ) targs in
            let v = Ttyp_variable (V.create ()) in
            unify decl.typ (Ttyp_function (args_type, v));
            {
                typ = v;
                node = Texpr_apply (name, targs)
            }
    
    | Ast.Pexpr_neg (expr) ->
        type_expr Ast.Pexr_binary ({ range : dummy_range; node = Ast.Bsub }, Pexpr_constant (Cint (0)), expr)

    | _ -> raise (Error (e.range, "not yet implemented"))

let check_arity args expected_arity =
    (List.compare_length_with args expected_arity) = 0

let type_pattern genv lenv pattern = match pattern.node with
    | Ast.Ppattern_constant c ->
        type_of_constant c

    | Ast.Ppattern_variable name -> 
        let typ = Ttyp_variable (V.create ()) in
        {
            typ;
            node = Tpattern_variable (name);
        }

    | Ast.Ppattern_constructor (name, patterns) ->
        match Hashtbl.find_opt genv.items name with
        | Some (constructor) -> (
            (* First, we check if we both the pattern arity and the
               expected constructor arity are the same. *)
            if not (check_arity patterns constructor.arity) then (
                raise (Error (dummy_range, "mismatch arity"))
            );

            (* Then, we check if the patterns type match the
               constructor expected arguments type. *)
            let tpatterns = List.map (fun p -> 
                type_pattern genv lenv pattern
            ) patterns in
            List.iter2 (fun tpattern expected_type ->
                unify tpattern.typ expected_type
            ) tpatterns constructor.args;
            
            {
                typ = constructor.decl.typ;
                node = Tpattern_constructor (name, tpatterns);
            }
        )

        | None -> 
            raise (Error (dummy_range, "unknown constructor"))

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

let is_non_variable_pattern = function
    | Ppattern_variable _ -> false
    | Ppattern_variable _ -> false
    | _ -> true
    
    | _ -> true
    
(* Check if a function's equation is valid. *)
let type_function_equation genv lenv decl (expected_arity, expected_args_type, expected_ret_type) (name, patterns, body) =
    (* We start to check if the equation's arity is the same as 
       the expected arity. *)
    if (List.compare_length_with patterns expected_arity) = 0 then (
        raise (Error (decl.range, "arity mismatch"))
    );

    (* Then, we check if each pattern is correctly typed. *)
    let typed_patterns = List.map2 (fun pattern expected_type -> (
        let typed_pattern = type_pattern genv lenv pattern
        in unify typed_pattern.typ expected_type;
        typed_pattern
    )) patterns expected_args_type in ()

    (* TODO:
        (* We also check that there is at most one non-variable pattern
        and it is at the same position as the other non-variables patterns
        in the other equations. *)
        List.iteri (fun i pattern -> (
        if is_non_variable_pattern pattern & i <> non_variable_index then
                raise (Error (pattern.range, "non variable pattern"))
        )) patterns
    *)

let type_function genv lenv ((decl : Ast.decl), (equations : Ast.decl list)) =
    let expected_args_type, expected_ret_type = match decl with
        | Pdecl_function (_, _, args_type, ret_type, _) ->
            List.map from_ast_type args_type, from_ast_type ret_type
        | _ -> assert false
    in
    (* Check if all function's equations are correctly typed. *)
    let typed_equations = List.map (fun eq -> 
        type_function_equation genv lenv decl
    ) equations
    in
    (* And if yes, compress all of them i
    | Ppattern_variable _ -> false
    | _ -> true
    nto a single function expression. 
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
    ()


let find_in_env genv locenv key = 
    try Hashtbl.find genv key
    with Not_found -> try Hashtbl.find locenv key
                      with Not_found -> raise (Error (dummy_ident, "type not found"))
(*Add Ttyp data in Ast?*)
let type_data genv lenv decl = match decl with
    | Pdecl_data (name, args, fields) -> (
        let loc_env = Hashtbl.create 17 in
        let list_typ = ref [] in
        List.iter (fun x -> let new_typ = Ttyp_variable (V.create()) in list_typ := new_typ :: (!list_typ); Hashtbl.add loc_env x args)
        Hashtbl.add genv.items name (Tdecl_data (name, !list_typ));
        (*We go through the list of fields in order to declare function that have the argument given and a return type of the data*)
        let rec add_fields l = match l with
            | [] -> ()
            | (name_cons, args) :: r -> Hashtbl.add genv.func name_cons Ttyp_func ( (List.iter (fun x -> find_in_env genv locenv x) args) (find_in_env genv locenv name)) in(*We must return a Ttyp_func (typ list of all the argument given, typ return which is the data type declare just above) *)
        add_fields fields
    )   

    | _ -> assert false


let type_class genv decl = match decl with
    | Pdecl_class (name, args, decls) -> 
        let loc_env = Hashtbl.create 17 in
        List.iter
            (
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


(*When we define a calss C a1 .. an, we can use it in a instance with instance C Int.abs
   An instance is either a ntype, either a ntype => ntype, either 
   We can also have fatarrow in function declaration
   When declaring an instance, when can add a list of nype like instance name ( ntype)
   In purescript, instance can have names but it's never possible in minipurescript
   We must also check that all general types declared are used.
   Should we check that an instance declaration 
   *)

let type_instances decl_inst genv loc_env = match inst with
    | Pdecl_instance (instance (inst))


let file decls = 
    let genv = { items = Hashtbl.create 17 } in
    List.iter (fun decl -> (
        match decl with
            | Pdecl_data (name, args, fields) ->
                type_data decl

            | _ -> ()
    )) decls

(* Returns a type (of the typed tree) corresponding to the given
   function declaration. *)
let type_from_function_decl genv (decl : Ast.decl) =
    let lenv = Hashtbl.create 17 in
    match decl.node with
    | Pdecl_function (_, vars, _, args_typ, return_typ) -> (
        List.iter (fun x -> Hashtbl.add x (V.create ())) vars;
        let link_type_param l = match l with
            | [] -> []
            | tp :: r -> try (Hashtbl.find lenv tp) :: (link_type_param r)
                         with Not_found -> (try (Hashtbl.find genv tp) :: (link_type_param r) 
                                            with Not_found -> raise (Error (Location.dummy, Location.dummy), "Type inconnu")) in
        Ttyp_function ((link_type_param args_typ),
                        try (Hashtbl.find lenv return_typ)
                        with Not_found -> (try (Hashtbl.find genv tp) 
                                           with Not_found -> raise (Error (Location.dummy, Location.dummy), "Type inconnu"))
        
                      )
    )                      

    | _ -> assert false