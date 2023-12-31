(* AST of MiniPureScript *)

exception Semantic_error of (Location.t * Location.t) * string

type 'a located_node = { range : Location.range; node : 'a }

(* a MiniPureScript program *)
type program = decl list

(* an identifier with source code location data *)
and ident = { spelling : string; ident_range : Location.range }
and decl = { decl_kind : decl_kind; decl_range : Location.range }

and decl_kind =
  (* a function equation *)
  | Pdecl_equation of
      ident (* function name *)
      * pattern list (* arguments *)
      * expr (* function body *)
  (* a function declaration *)
  | Pdecl_function of
      ident (* function name *)
      * ident list (* quantified variables *)
      * typ list (* class types (constraints) *)
      * typ list (* arguments type *)
      * typ (* return type *)
  (* a data declaration *)
  | Pdecl_data of
      ident (* data name *)
      * ident list (* arguments *)
      * constructor list (* constructors *)
  (* a class declaration *)
  | Pdecl_class of
      ident (* class name *)
      * ident list (* arguments *)
      * decl list (* class declarations (fields) *)
  (* an instance declaration *)
  | Pdecl_instance of
      schema (* instance schema *)
      * decl list (* instance declarations (fields) *)

(* a data constructor *)
and constructor = ident * typ list
and typ = { type_kind : typ_kind; type_range : Location.range }

and typ_kind =
  (* a variable type (in lowercase) *)
  | Ptyp_variable of string
  (* a data type with the name and its arguments *)
  | Ptyp_data of ident * typ list

(* <ntype> OR (<ntype>+) => <ntype> *)
and class_type = ident * typ list

and schema =
  class_type list (* instance's dependencies *)
  * class_type (* instance's target *)

and constant =
  (* e.g. true or false *)
  | Cbool of bool
  (* e.g. 42 *)
  | Cint of int
  (* e.g. "foo bar" *)
  | Cstring of string
  (* e.g. "unit" *)
  | Cunit

and expr = { expr_kind : expr_kind; expr_range : Location.range }

and expr_kind =
  (* constant *)
  | Pexpr_constant of constant
  (* binary operator, lhs, rhs *)
  | Pexpr_binary of binop * expr * expr
  (* sub expression, arithmetic negation *)
  | Pexpr_neg of expr
  (* variable name *)
  | Pexpr_variable of ident
  (* function name, arguments *)
  | Pexpr_apply of ident * expr list
  (* constructor name, arguments *)
  | Pexpr_constructor of ident * expr list
  (* condition expression, then expression, else expression *)
  | Pexpr_if of expr * expr * expr
  (* block of multiple expressions *)
  | Pexpr_do of expr list
  (* let bindings in expression *)
  | Pexpr_let of binding list * expr
  (* condition expression, the branches *)
  | Pexpr_case of expr * branch list

(* <lident> = <expr> *)
and binding = ident * expr
and pattern = { pattern_kind : pattern_kind; pattern_range : Location.range }

and pattern_kind =
  (* constant *)
  | Ppattern_constant of constant
  (* variable name *)
  | Ppattern_variable of ident
  (* constructor name, arguments to constructor *)
  | Ppattern_constructor of ident * pattern list

(* <pattern> -> <expr> *)
and branch = pattern * expr
and binop = binop_kind located_node

and binop_kind =
  (* + - * / *)
  | Badd
  | Bsub
  | Bmul
  | Bdiv
  (* == /= < <= > >= *)
  | Beq
  | Bneq
  | Blt
  | Ble
  | Bgt
  | Bge
  (* && || <> *)
  | Band
  | Bor
  | Bconcat
