(* AST of MiniPureScript *)

exception Semantic_error of string

type lident = string
type uident = string

type 'a located_node =
    {
        start_loc : Location.t;
        end_loc : Location.t;
        node : 'a;
    }

type file = Fprogram of decl list

and decl =
    | DECLdefn of defn
    | DECLtdecl of tdecl
    | DECLdata of uident * (lident list) * (located_typ list) (* data <uident> <lident>* = (<uident> <atype>* )+ *)
    | DECLclass of uident * (lident list) * (tdecl list) (* class <uident> <lident>* where { <tdecl>*; } *)
    | DECLinstance of instance * (defn list) (* instance <instance> where { <defn>*; } *)

and defn = DEF of lident * (located_pattern list) * located_expr (* <lident> <partag>* = expr *)

and tdecl = TDECL of lident * (lident list) * (typ list) * (typ list) * typ (* <lident> :: (forall <lident>+ .)?
                                                                                (<ntype> =>)* (<type> ->)* <type> *)
and located_typ = typ located_node
and typ =
    | Ptyp_variable of string
    | Ptyp_apply of string located_node * located_typ list

and instance =
    (* <ntype> *)
    | Pinstance of located_typ
    (* (<ntype>+) => <ntype> *)
    | Pinstance_dep of located_typ list * located_typ

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
and located_expr = expr located_node
and expr =
    (* <constant> *)
    | Pexpr_constant of constant
    (* <expr> <op> <expr> *)
    | Pexpr_binary of binop located_node * located_expr * located_expr
    (* - <expr> *)
    | Pexpr_neg of located_expr
    | Pexpr_apply of string located_node * located_expr list
    (* if e1 then e2 else e3 *)
    | Pexpr_if of located_expr * located_expr * located_expr
    (* do <exprs> *)
    | Pexpr_do of located_expr list
    (* let <bindings> in <expr> *)
    | Pexpr_let of binding list * located_expr
    (* case <expr> of <branchs> *)
    | Pexpr_case of located_expr * branch list

(* <lident> = <expr> *)
and binding = string located_node * located_expr

and located_pattern = pattern located_node
and pattern =
    (* e.g. 42 *)
    | Ppattern_constant of constant
    (* e.g. foo *)
    | Ppattern_variable of string
    (* e.g. Bar 42 *)
    | Ppattern_apply of string located_node * located_pattern list

(* <pattern> -> <expr> *)
and branch = located_pattern * located_expr

and binop =
    (* + - * / *)
    | Badd | Bsub | Bmul | Bdiv
    (* == /= < <= > >= *)
    | Beq | Bneq | Blt | Ble | Bgt | Bge
    (* && || <> *)
    | Band | Bor | Bconcat
