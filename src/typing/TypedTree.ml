(* Typed tree of MiniPureScript *)

type typ =
  (* a quantified type variable, to be unified. *)
  | Ttyp_variable of tvar
  (* type of a data declaration *)
  | Ttyp_data of string * typ list

and tvar = { id : int; mutable def : typ option }
and 'a typed_node = { typ : typ; range : Location.range; node : 'a }
and decl = decl_kind typed_node

and decl_kind =
  (* a function declaration *)
  | Tdecl_function of
      Ast.ident (* function name *)
      * param list (* parameters *)
      * expr (* function body *)
  (* a data declaration *)
  | Tdecl_data of
      Ast.ident (* data name *) * constructor list (* constructors *)
  | Tdecl_class of Ast.ident (* class name *)

(* a data constructor *)
and constructor = Ast.ident * typ list

(* a function parameter *)
and param = Ast.ident * typ
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
  | Texpr_apply of Ast.ident * expr list
  (* constructor name, arguments *)
  | Texpr_constructor of Ast.ident * expr list
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
  (* _ *)
  | Tpattern_wildcard
  (* e.g. 42 *)
  | Tpattern_constant of Ast.constant
  (* e.g. foo *)
  | Tpattern_variable of Ast.ident
  (* e.g. Bar 42 *)
  | Tpattern_constructor of Ast.ident * pattern list

(* <pattern> -> <expr> *)
and branch = pattern * expr

type function_decl = {
  func_name : Ast.ident; (* function's name *)
  params : typ list; (* parameters types *)
  arity : int; (* number of parameters *)
  tvars : (string, typ) Hashtbl.t; (* type variables of the function *)
  retty : typ; (* return type of the function *)
}

type function_impl = {
  func_decl : function_decl; (* function's declaration *)
  func_expr : expr; (* funmction's body expression *)
}

and data_decl = {
  data_name : Ast.ident; (* data's name *)
  constructors : constructor_decl list; (* list of constructors *)
  data_tvars : (string, typ) Hashtbl.t; (* type variables of the data *)
  data_arity : int; (* number of type variables *)
  data_typ : typ; (* type of this data *)
}

and constructor_decl = {
  cons_name : Ast.ident; (* constructor's name *)
  data_decl : data_decl; (* the data to whom this constructor is attached *)
  args : typ list; (* expected arguments types *)
  arity : int; (* number of arguments *)
}

module SMap = Map.Make (String)
module SSet = Set.Make (String)

type class_decl = {
  class_name : Ast.ident; (* class's name *)
  class_tvars : (string, typ) Hashtbl.t; (* type variables of the class *)
  class_funcs : SSet.t;
}

module V = struct
  type t = tvar

  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id

  let create =
    let r = ref 0 in
    fun () ->
      incr r;
      { id = !r; def = None }
end

let unit_type = Ttyp_data ("Unit", [])
let boolean_type = Ttyp_data ("Boolean", [])
let int_type = Ttyp_data ("Int", [])
let string_type = Ttyp_data ("String", [])
let effect_type = Ttyp_data ("Effect", [ Ttyp_variable (V.create ()) ])
let effect_unit = Ttyp_data ("Effect", [ unit_type ])
