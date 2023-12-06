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

type 'a typed_node = 
    {
        (* The type of the tree node. *)
        typ : typ;
        (* The underlying node. *)
        node : 'a;
    }

and typed_expr = expr typed_node
and expr =
    (* A constant, like an integer, a boolean or a string. *)
    | Texpr_constant of Ast.constant
    (* <expr> <op> <expr> *)
    | Texpr_binary of Ast.binop * typed_expr * typed_expr
    (* <name> OR <name> <exprs> 
       This also include variable references. *)
    | Texpr_apply of string * (typed_expr list)
    (* if <expr> then <expr> else <expr> *)
    | Texpr_if of typed_expr * typed_expr * typed_expr
    (* do <exprs> *)
    | Texpr_do of typed_expr list
    (* let <binding> = <expr> *)
    | Texpr_let of binding * typed_expr
    (* case <expr> of <branches> *)
    | Texpr_case of typed_expr * pattern list

(* <lident> = <expr> *)
and binding = string * typed_expr

and typed_pattern = pattern typed_node
and pattern =
    (* e.g. 42 *)
    | Tpattern_constant of Ast.constant
    (* e.g. foo *)
    | Tpattern_variable of string
    (* e.g. Bar 42 *)
    | Tpattern_apply of string * typed_pattern list

(* <pattern> -> <expr> *)
and branch = typed_pattern * typed_expr
