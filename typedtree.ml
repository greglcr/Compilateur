(* Typed tree of MiniPureScript *)

(* The different builtin types supported by MiniPureScript. *)
type builtin_type =
    (* Unit *)
    | Tbuiltin_type_unit
    (* Boolean *)
    | Tbuiltin_type_boolean
    (* Int *)
    | Tbuiltin_type_int
    (* String *)
    | Tbuiltin_type_string
    (* Effect a *)
    | Tbuiltin_type_effect

let builtin_type_from_symbol s = match s with
    | "Unit" -> Some Tbuiltin_type_unit
    | "Boolean" -> Some Tbuiltin_type_boolean
    | "Int" -> Some Tbuiltin_type_int
    | "String" -> Some Tbuiltin_type_string
    | "Effect" -> Some Tbuiltin_type_effect
    | _ -> None

type typ =
    | Ttyp_unit
    | Ttyp_boolean
    | Ttyp_int
    | Ttyp_string
    | Ttyp_effect of typ
    | Ttyp_var of tvar
    | Ttyp_function of typ list * typ

and tvar = 
    {
        id : int;
        mutable def : typ option;
    }

and 'a typed_node = 
    {
        (* The type of the tree node. *)
        typ : typ;
        (* The underlying node. *)
        node : 'a;
    }

and decl = decl_node typed_node
and decl_node =
    | Tdecl_function of Ast.ident * param list * expr

and param = string typed_node

and expr = expr_node typed_node
and expr_node =
    (* A constant, like an integer, a boolean or a string. *)
    | Texpr_constant of Ast.constant
    (* <expr> <op> <expr> *)
    | Texpr_binary of Ast.binop * expr * expr
    (* <name> OR <name> <exprs> 
       This also include variable references. *)
    | Texpr_apply of Ast.ident * (expr list)
    (* if <expr> then <expr> else <expr> *)
    | Texpr_if of expr * expr * expr
    (* do <exprs> *)
    | Texpr_do of expr list
    (* let <binding> = <expr> *)
    | Texpr_let of binding * expr
    (* case <expr> of <branches> *)
    | Texpr_case of expr * pattern list

(* <lident> = <expr> *)
and binding = Ast.ident * expr

and pattern = pattern_node typed_node
and pattern_node =
    (* e.g. 42 *)
    | Tpattern_constant of Ast.constant
    (* e.g. foo *)
    | Tpattern_variable of string
    (* e.g. Bar 42 *)
    | Tpattern_apply of string * pattern list

(* <pattern> -> <expr> *)
and branch = pattern * expr
