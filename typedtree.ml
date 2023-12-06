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
    | Tunit
    | Tboolean
    | Tint
    | Tstring
    | Teffect of typ

type 'a typed_node = 
    {
        typ: typ;
        node: 'a;
    }

and typed_expr = expr typed_node

and expr =
    | Texp_constant of Ast.constant
    | Texp_binary of Ast.binop * typed_expr * typed_expr
    | Texp_if of typed_expr * typed_expr * typed_expr
    | Texp_do of typed_expr list
