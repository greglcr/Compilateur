(* Typed tree of MiniPureScript *)

type typ =
    (* type of the "unit" constant *)
    | Ttyp_unit
    (* type of "true" and "false" *)
    | Ttyp_boolean
    (* type of integer constants *)
    | Ttyp_int
    (* type of string constants *)
    | Ttyp_string
    (* builtin data type Effect *)
    | Ttyp_effect of typ
    (* a quantified type variable, to be unified. *)
    | Ttyp_variable of tvar
    (* type of a function (constructors are not functions) *)
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

and decl = decl_kind typed_node
and decl_kind =
    | Tdecl_function of Ast.ident * param list * expr

(* a function parameter *)
and param = string * typ

and expr = expr_kind typed_node
and expr_kind =
    (* constant *)
    | Texpr_constant of Ast.constant
    (* binary operator, lhs, rhs *)
    | Texpr_binary of Ast.binop * expr * expr
    (* Pexpr_neg has no equivalent in the typed tree because it
       is converted to the equivalent "0 - e" binary expression. *)
    (* variable name *)
    | Texpr_variable of Ast.ident
    (* function name, arguments *)
    | Texpr_apply of Ast.ident * (expr list)
    (* constructor name, arguments *)
    | Texpr_constructor of Ast.ident * (expr list)
    (* condition expression, then expression, else expression *)
    | Texpr_if of expr * expr * expr
    (* block of multiple expressions *)
    | Texpr_do of expr list
    (* let bindings in expression *)
    | Texpr_let of binding * expr
    (* condition expression, the branches *)
    | Texpr_case of expr * branch list

(* <lident> = <expr> *)
and binding = Ast.ident * expr

and pattern = pattern_kind typed_node
and pattern_kind =
    (* e.g. 42 *)
    | Tpattern_constant of Ast.constant
    (* e.g. foo *)
    | Tpattern_variable of Ast.ident
    (* e.g. Bar 42 *)
    | Tpattern_constructor of Ast.ident * pattern list

(* <pattern> -> <expr> *)
and branch = pattern * expr

type function_decl =
    {
        (* The function's name. *)
        name : string;
        (* Quantified type variables (the variables after the forall.). *)
        tvars : (string, typ) Hashtbl.t;
        (* Arguments type of the function eventually referencing types in tvars. *)
        args : typ list;
        (* Arity of the function, i.e. List.length args *)
        arity : int;
    }

type constructor_decl =
    {
        (* The constructor's name. *)
        name : string;
    }
