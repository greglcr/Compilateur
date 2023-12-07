(* AST of MiniPureScript *)

exception Semantic_error of string

type 'a located_node =
    {
        start_loc : Location.t;
        end_loc : Location.t;
        node : 'a;
    }

type file = decl list

and ident = string located_node

and decl = decl_node located_node
and decl_node =
    (* Function declaration 
       <lident> <partag>* = expr *)
    | Pdecl_func of ident * (pattern list) * expr
    (* Function signature 
       <lident> :: (forall <lident>+ .)? (<ntype> =>)* (<type> ->)* <type> *)
    | Pdecl_func_signature of ident * (ident list) * (typ list) * (typ list) * typ
    (* Data declaration
       data <uident> <lident>* = (<uident> <atype>* )+ *)
    | Pdecl_data of ident * (ident list) * (typ list) 
    (* Class declaration
       class <uident> <lident>* where { <tdecl>*; } *)
    | Pdecl_class of ident * (ident list) * (decl list) 
    (* Instance declaration
       instance <instance> where { <defn>*; } *)
    | Pdecl_instance of instance * (decl list)

and typ = typ_node located_node
and typ_node =
    | Ptyp_variable of string
    | Ptyp_apply of ident * typ list

(* <ntype> OR (<ntype>+) => <ntype> *)
and instance = typ list * typ option

and constant =
    (* e.g. true or false *)
    | Cbool of bool
    (* e.g. 42 *)
    | Cint of int
    (* e.g. "foo bar" *)
    | Cstring of string

(* Some notes:
   - A variable reference, e.g. "foo", is represented as function call without
     any argument.
   - A module or constructor call is the same as an function call.
*)
and expr = expr_node located_node
and expr_node =
    (* <constant> *)
    | Pexpr_constant of constant
    (* <expr> <op> <expr> *)
    | Pexpr_binary of binop located_node * expr * expr
    (* - <expr> *)
    | Pexpr_neg of expr
    | Pexpr_apply of ident * expr list
    (* if e1 then e2 else e3 *)
    | Pexpr_if of expr * expr * expr
    (* do <exprs> *)
    | Pexpr_do of expr list
    (* let <bindings> in <expr> *)
    | Pexpr_let of binding list * expr
    (* case <expr> of <branchs> *)
    | Pexpr_case of expr * branch list

(* <lident> = <expr> *)
and binding = ident * expr

and pattern = pattern_node located_node
and pattern_node =
    (* e.g. 42 *)
    | Ppattern_constant of constant
    (* e.g. foo *)
    | Ppattern_variable of string
    (* e.g. Bar 42 *)
    | Ppattern_apply of ident * pattern list

(* <pattern> -> <expr> *)
and branch = pattern * expr

and binop =
    (* + - * / *)
    | Badd | Bsub | Bmul | Bdiv
    (* == /= < <= > >= *)
    | Beq | Bneq | Blt | Ble | Bgt | Bge
    (* && || <> *)
    | Band | Bor | Bconcat
