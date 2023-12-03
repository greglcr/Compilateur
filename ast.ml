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
  | DECLdata of uident * (lident list) * (uident * (atype list)) list (* data <uident> <lident>* = (<uident> <atype>* )+ *)
  | DECLclass of uident * (lident list) * (tdecl list) (* class <uident> <lident>* where { <tdecl>*; } ici les deux listes
                                                      peuvent être éventuellement vides *)
  | DECLinstance of instance * (tdecl list) (* instance <instance> where { <defn>*; } *)

and defn = DEF of lident * (patarg list) * expr (* <lident> <partag>* = expr *)

and tdecl = TDECL of lident * (lident list) * (ntype list) * (tp list) * tp (* <lident> :: (forall <lident>+ .)?
                                                                                (<ntype> =>)* (<type> ->)* <type> *)

and ntype = NTP of uident * (atype list) (* <uident> <atype>* *)

and atype = 
  | ATl of lident
  | ATu of uident
  | ATt of tp (* (<type>) *)  

and tp = 
  | TPa of atype
  | TPn of ntype

and instance = 
  | INSTntp of ntype
  | INSTntpc of ntype * ntype (* <ntype> => <ntype> *)
  | INSTntpcc of (ntype list) * ntype (* ( <ntype>+ ) => <ntype> *)

and patarg = 
  | PATARconst of constant
  | PATARlid of lident
  | PATARuid of uident
  | PATARpat of pattern (* (<pattern>) *)
  
and pattern = 
  | PATERpatar of patarg
  | PATERjspquelnom of uident * (patarg list)

and constant =
  | Cbool of bool
  | Cint of int
  | Cstring of string

and atom = 
  | Aconst of constant
  | Alident of lident
  | Auident of uident
  | Aexpr of expr (* Pour les ( <expr> ) *)
  | Aexprtype of expr * tp

and expr = 
  | Eatom of atom
  | Ebinop of binop * expr * expr (* On rajoute pas l'opérateur unaire -, on le verra comme -v = Ebinop(Bsub, 0, v) *)
  | Efonct of lident * (atom list)
  | Emodule of uident * (atom list) (* Dans les deux cas la liste ne peux pas être vide *)
  | Econd of expr * expr * expr (* if e1 then e2 else e3 *)
  | Edo of (expr list) (* do { <expr>+ ;} *)
  | Eaffect of (binding list) * expr (* let { <bindings>+ } in <expr> *)
  | Ecase of expr * (branch list) (* case <expr> of { <branch>+; } *)

and binding = Baffect of lident * expr (* <lident> = <expr> *)

and branch = Barrow of pattern * expr (* <pattern> -> <expr>  *)

and binop =
  | Badd | Bsub | Bmul | Bdiv           (* + - * / *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == /= < <= > >= *)
  | Band | Bor | Bconcat                (* && || <> *)