{
    open Lexing
    open Parser
    open Ast

    exception Lexing_error of string


    let find_valeur = 
        let linkMap = Hashtbl.create 17 in
        List.iter (fun (s, l) -> Hashtbl.add linkMap s l)
                  (
                    [
                        ("case", CASE);
                        ("class", CLASS);
                        ("data", DATA);
                        ("do", DO);
                        ("else", ELSE);
                        ("false", CST (Cbool (false)));
                        ("forall", FORALL);
                        ("if", IF);
                        ("import", IMPORT);
                        ("in", IN);
                        ("instance", INSTANCE);
                        ("let", LET);
                        ("module", MODULE);
                        ("of", OF);
                        ("then", THEN);
                        ("true", CST (Cbool (true)));
                        ("where", WHERE);

                        ("+", BINOP (Badd) );
                        ("-", BINOP (Bsub) );
                        ("*", BINOP (Bmul) );
                        ("/", BINOP (Bdiv) );
                        ("<>", BINOP (Bneq) );
                        ("&&", BINOP (Band) );
                        ("||", BINOP (Bor) );
                        ("==", BINOP (Beq) );
                        ("<", BINOP (Blt) );
                        ("<=", BINOP (Ble) );
                        (">", BINOP (Bgt) );
                        (">=", BINOP (Bge) );

                        ("=", EQUAL);

                        ("(", LEFTPAR);
                        (")", RIGHTPAR);
                        ("{", LEFTBRACE);
                        ("}", RIGHTBRACE);
                        (";", SEMICOLON)
                    ]
                  );
        fun s -> try Hashtbl.find linkMap s with Not_found -> LIDENT s
}

let digit = ['0'-'9']
let lower = ['a'-'z' '_']
let upper = ['A'-'Z']
let mathexpr = "+" | "-" | "*" | "/" | "<>" | "&&" | "||" | "==" | "<" | "<=" | ">" | ">=" | "="
let sep = "(" | ")" | "{" | "}" | ";" | "."
let other = lower | upper | digit | "'"
let lident = lower other*  
let uident = upper (other | '.')*

rule next_token = parse
    | "module Main where"
        { FILEINIT }

    | "import Prelude\nimport Effect\nimport Effect.Console"
        { IMPORTINIT }

    | ['\n' '\r' '\t' ' ']
        { next_token lexbuf }

    | lower+ | mathexpr | sep as id
        { find_valeur id }

    | digit+ as n
        { CST (Cint (int_of_string n) ) }
    
    | lident as l
        { LIDENT (l) }
    
    | uident as u
        { UIDENT (u) }
    
    | "--"
        { comment_line lexbuf}

    | "{-"
        { comment lexbuf }
    
    | eof
        { EOF }

    | _
        { raise (Lexing_error ("erreur") ) }

and comment_line = parse
    | '\n' 
        { next_token lexbuf }

    | eof
        { raise (Lexing_error ("Commentaire non terminé")) }

    | _
        { comment_line lexbuf }

and comment = parse
    | "-}"
        { next_token lexbuf }
    
    | eof
        { raise (Lexing_error ("Commentaire non terminé")) }

    | _
        { comment lexbuf }

{

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | BINOP _ ->
        "BINOP"
    | CASE ->
        "CASE"
    | CLASS ->
        "CLASS"
    | CST _ ->
        "CST"
    | DATA ->
        "DATA"
    | DO ->
        "DO"
    | ELSE ->
        "ELSE"
    | EOF ->
        "EOF"
    | EQUAL ->
        "EQUAL"
    | FALSE ->
        "FALSE"
    | FILEINIT ->
        "FILEINIT"
    | FORALL ->
        "FORALL"
    | IDENT _ ->
        "IDENT"
    | IF ->
        "IF"
    | IMPORT ->
        "IMPORT"
    | IMPORTINIT ->
        "IMPORTINIT"
    | IN ->
        "IN"
    | INSTANCE ->
        "INSTANCE"
    | LEFTBRACE ->
        "LEFTBRACE"
    | LEFTPAR ->
        "LEFTPAR"
    | LET ->
        "LET"
    | LIDENT _ ->
        "LIDENT"
    | MODULE ->
        "MODULE"
    | OF ->
        "OF"
    | RIGHTBRACE ->
        "RIGHTBRACE"
    | RIGHTPAR ->
        "RIGHTPAR"
    | SEMICOLON ->
        "SEMICOLON"
    | THEN ->
        "THEN"
    | TRUE ->
        "TRUE"
    | UIDENT _ ->
        "UIDENT"
    | WHERE ->
        "WHERE"

    let print_lexeme fileName =
        let c = open_in fileName in
        let lb = from_channel c in
        let rec liste_lexemes curTok = match curTok with
            | EOF -> print_string "EOF\n"
            | _ -> print_string (_menhir_print_token curTok); print_string " "; (liste_lexemes (next_token lb)) in
        (liste_lexemes (next_token lb));;

}



    
    
