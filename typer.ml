open Typedtree

exception Error of string

let effect_unit = Ttyp_effect Ttyp_unit

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

let rec expr = function
    | Ast.Econst c -> (
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

    | Ast.Ebinop (op, lhs, rhs) -> (
        let tlhs = expr lhs in
        let trhs = expr rhs in

        let t = match op with
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
            typ = (Ttyp_effect Ttyp_unit);
            node = Texpr_do texprs;
        }
    )

    | _ -> raise (Error "not yet implemented")

let rec decl = function
    | _ -> ()

let file = function
    | Ast.Fprogram decls -> List.map decl decls
