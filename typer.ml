open Typedtree

exception Error of string

let check_if_unit expr = 
    if expr.typ <> Tunit then
        raise (Error "expected Effect Unit type")

let check_if_int expr = 
    if expr.typ <> Tint then
        raise (Error "expected Int type")

let check_if_string expr = 
    if expr.typ <> Tstring then
        raise (Error "expected String type")

let check_if_boolean expr = 
    if expr.typ <> Tboolean then
        raise (Error "expected Boolean type")

let check_if_has_eq expr = 
    if (expr.typ <> Tboolean) || (expr.typ <> Tint) || (expr.typ <> Tstring) then
        raise (Error "expected Boolean, Int or String type")

let rec expr = function
    | Ast.Econst c -> (
        let t = match c with
            | Cbool _ -> Tboolean
            | Cint _ -> Tint
            | Cstring _ -> Tstring
        in
        {
            typ = t;
            node = Texpr_constant c;
        }
    )

    | Ast.Ebinop (op, lhs, rhs) -> (
        let tlhs = expr lhs in
        let trhs = expr rhs in

        let t = match op with
            | Ast.Badd | Ast.Bsub | Ast.Bmul | Ast.Bdiv -> (
                check_if_int tlhs;
                check_if_int trhs;
                Tint
            )
            | Ast.Bconcat -> (
                check_if_string tlhs;
                check_if_string trhs;
                Tstring
            )
            | Ast.Beq | Ast.Bneq -> (
                check_if_has_eq tlhs;
                check_if_has_eq trhs;
                Tboolean
            )
            | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge -> (
                check_if_int tlhs;
                check_if_int trhs;
                Tboolean
            )
            | Ast.Bor | Ast.Band -> (
                check_if_boolean tlhs;
                check_if_boolean trhs;
                Tboolean
            )
        in

        {
            typ = t;
            node = Texpr_binary (op, tlhs, trhs);
        }
    )

    | Ast.Eif (cond, then_, else_) -> (
        let tcond = expr cond in
        let tthen = expr then_ in
        let telse = expr else_ in

        if tthen <> telse then (
            raise (Error "else branch must have the same type as the then branch")
        );

        { 
            typ = tthen.typ;
            node = Texpr_if (tcond, tthen, telse);
        }
    )

    | Ast.Edo exprs -> (
        let texprs = List.map expr exprs in
        List.iter check_if_unit texprs;
        
        { 
            typ = Tunit;
            node = Texpr_do texprs;
        }
    )

    | _ -> raise (Error "not yet implemented")

let rec decl = function
    | _ -> ()

let file = function
    | Ast.Fprogram decls -> List.map decl decls
