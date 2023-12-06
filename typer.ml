open Typedtree

exception Error of string

module SMap = Map.Make(String)

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
        let texprs = List.map (fun (e : Ast.located_expr) -> expr state e.node) exprs in
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

let rec decl = function
    | _ -> ()

let file = function
    | Ast.Fprogram decls -> List.map decl decls
