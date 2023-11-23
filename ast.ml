(*Syntaxe abstraite de minipurescript*)

type ident =
  | Lident of string
  | Uident of string

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor

type constant =
  | Cbool of bool
  | Cint of int
  | Cstring of string

(*

type atom = 
  | Aconst of constant
  | Alident of string
  | Auident of string
  | Aexpr of expr
  | 

*)


type expr = 
  | Econstant of constant
  | Eident of ident
  | Ebinop of binop * expr * expr
  