open Typedtree
open Ast

exception Error of string

module SMap = Map.Make(String)
module SSet = Set.Make(String)

type env =
    {
        decls : (string, Ast.decl) Hashtbl.t
    }

let effect_unit = Ttyp_effect (Ttyp_unit)
let check_if_unit expr = 
    if expr.typ <> effect_unit then
        raise (Error "expected Effect Unit type")

let check_if_int expr = 
    if expr.typ <> Ttyp_int then
        raise (Error "expected Int type")

let check_if_string expr = 
    if expr.typ <> Ttyp_string then
        raise (Error "expected String type")

let check_if_boolean expr = 
    if expr.typ <> Ttyp_boolean then
        raise (Error "expected Boolean type")

let check_if_has_eq expr = 
    if (expr.typ <> Ttyp_boolean) || (expr.typ <> Ttyp_int) || (expr.typ <> Ttyp_string) then
        raise (Error "expected Boolean, Int or String type")

let rec expr state = function
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
        let tlhs = expr state lhs.node in
        let trhs = expr state rhs.node in

        let t = match op.node with
            | Ast.Badd | Ast.Bsub | Ast.Bmul | Ast.Bdiv -> (
                check_if_int tlhs;
                check_if_int trhs;
                Ttyp_int
            )
            | Ast.Bconcat -> (
                check_if_string tlhs;
                check_if_string trhs;
                Ttyp_string
            )
            | Ast.Beq | Ast.Bneq -> (
                check_if_has_eq tlhs;
                check_if_has_eq trhs;
                Ttyp_boolean
            )
            | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge -> (
                check_if_int tlhs;
                check_if_int trhs;
                Ttyp_boolean
            )
            | Ast.Bor | Ast.Band -> (
                check_if_boolean tlhs;
                check_if_boolean trhs;
                Ttyp_boolean
            )
        in

        {
            typ = t;
            node = Texpr_binary (op.node, tlhs, trhs);
        }
    )

    | Ast.Pexpr_if (cond, then_, else_) -> (
        let tcond = expr state cond.node in
        let tthen = expr state then_.node in
        let telse = expr state else_.node in

        if tthen <> telse then (
            raise (Error "else branch must have the same type as the then branch")
        );

        { 
            typ = tthen.typ;
            node = Texpr_if (tcond, tthen, telse);
        }
    )

    | Ast.Pexpr_do exprs -> (
        let texprs = List.map (fun (e : Ast.expr) -> expr state e.node) exprs in
        List.iter check_if_unit texprs;
        
        { 
            typ = Ttyp_unit;
            node = Texpr_do texprs;
        }
    ) 

    | Ast.Pexpr_let (bl, e) -> (
        match bl with
            | [] -> expr state e.node
            | (l, vl) :: r -> let tb = expr state vl.node in
                              let te = expr (SMap.add l.node tb.typ state) (Ast.Pexpr_let (r, e)) in
                              {
                                typ = te.typ;
                                node = Texpr_let ((l.node, tb), te);
                              }

                            
    )
    
    | _ -> raise (Error "not yet implemented")

let rec find_functions functions decls =
    let handle_decl decl = match decl with
        | Pdecl_func (name, patterns, expr) -> (
            Hashtbl.add functions name.node decl
        )
        | Pdecl_func_signature _ -> ()
        | _ -> ()
    in match decls with
    | [] -> functions
    | decl::remaining -> (handle_decl decl.node); find_functions  functions remaining

let type_func functions name =
    let all_declarations = Hashtbl.find_all functions name in
    (*
       We use the following algorithm to simplify functions declarations :
       - If there is a single declaration for the function => We do nothing.
       - If not, we compress all the different declarations in a single function
         declaration using the case expression. For example, if we have :
            f a = e1
            f b = e2
            f c = e3

         Then we "compress" it to :
            f e =
                case e of
                | a -> e1
                | b -> e2
                | c -> e3

         However, if two declarations does not have the same amount of patterns
         or if at least one declaration have more than 1 pattern then we
         emit an error. For example :
            f a b = e1
            f c d = e2
            -- ERROR: we can not compress the two declarations into one
    *)

let file = function
    | Ast.Fprogram decls -> 
        let functions = find_functions (Hashtbl.create 31) decls in
        let functions_name = SSet.of_seq (Hashtbl.to_seq_keys functions) in
        SSet.iter (fun name -> type_func functions name) functions_name
