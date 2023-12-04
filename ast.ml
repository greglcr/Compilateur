(* Syntaxe abstraite de minipurescript *)

type lident = string
type uident = string

(* Vu que les import sont toujours pareil, il n'y a pas besoin de créer un type import, on voit seulement
   le type file comme une suite de déclaration. La gestion des imports va être ajoutée directement dans les règles
   de grammaire *)

type file = Fprogramm of decl list (* module Main where { ⟨imports⟩ ⟨decl⟩+; } EOF, donc la liste doit être non
                                      vide *)

and decl =
    | DECLdefn of defn
    | DECLtdecl of tdecl
    | DECLdata of uident * (lident list) * (uident * (typ list)) list (* data <uident> <lident>* = (<uident> <atype>* )+ *)
    | DECLclass of uident * (lident list) * (tdecl list) (* class <uident> <lident>* where { <tdecl>*; } ici les deux listes
                                                        peuvent être éventuellement vides *)
    | DECLinstance of instance * (tdecl list) (* instance <instance> where { <defn>*; } *)

and defn = DEF of lident * (pattern list) * expr (* <lident> <partag>* = expr *)

and tdecl = TDECL of lident * (lident list) * (typ list) * (typ list) * typ (* <lident> :: (forall <lident>+ .)?
                                                                                (<ntype> =>)* (<type> ->)* <type> *)
and typ =
    | Tvar of string
    | Tsymbol of string * typ list

and instance =
    | INSTntp of typ
    | INSTntpc of typ * typ (* <ntype> => <ntype> *)
    | INSTntpcc of (typ list) * typ (* ( <ntype>+ ) => <ntype> *)

and pattern =
    | Pconst of constant
    | Pvar of string
    | Papp of string * pattern list

and constant =
    | Cbool of bool
    | Cint of int
    | Cstring of string

(* Some notes:
   - The unary operator "-e" is represented as the expression "0 - e". 
   - A variable reference, e.g. "foo", is represented as function call without 
     any argument.
   - A module or constructor call is the same as an function call.
*)
and expr =
    | Econst of constant
    | Ebinop of binop * expr * expr
    | Eapp of string * (expr list)
    | Eif of expr * expr * expr       (* if e1 then e2 else e3 *)
    | Edo of (expr list)              (* do { <exprs> } *)
    | Elet of (binding list) * expr   (* let { <bindings> } in <expr> *)
    | Ecase of expr * (branch list)   (* case <expr> of { <branchs> } *)

and binding = Baffect of lident * expr (* <lident> = <expr> *)

and branch = Barrow of pattern * expr (* <pattern> -> <expr>  *)

and binop =
    | Badd | Bsub | Bmul | Bdiv           (* + - * / *)
    | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == /= < <= > >= *)
    | Band | Bor | Bconcat                (* && || <> *)
