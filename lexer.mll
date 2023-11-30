{
    open Lexing
    open Parser
    open Ast

    exception Lexing_error of string

    let string_buffer = Buffer.create 1024

    let resolve_keyword =
        let keywords = Hashtbl.create 17 in
        List.iter (fun (s, l) -> Hashtbl.add keywords s l)
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
                        ("where", WHERE)
                    ]
                  );
        fun s -> try Hashtbl.find keywords s with Not_found -> LIDENT s
}

let digit = ['0'-'9']
let lower = ['a'-'z' '_']
let upper = ['A'-'Z']
let other = lower | upper | digit | "'"
let lident = lower other*
let uident = upper (other | '.')*
let eol = '\n' | '\r' | "\r\n"

rule next_token = parse
    | "module Main where"
        { FILEINIT }

    | "import Prelude\nimport Effect\nimport Effect.Console"
        { IMPORTINIT }

    | ['\t' ' ']
        { next_token lexbuf }

    | eol
        { new_line lexbuf; next_token lexbuf }

    | "--"
        { line_comment lexbuf}

    | "{-"
        { block_comment lexbuf }

    | digit+ as n
        { CST (Cint (int_of_string n) ) }
    
    | lident as l
        { resolve_keyword l }
    
    | uident as u
        { UIDENT (u) }

    | '"'
        { CST (Cstring (string lexbuf)) }

    | "+"
        { BINOP (Badd) }

    | "-"
        { BINOP (Bsub) }

    | "*"
        { BINOP (Bmul) }

    | "/"
        { BINOP (Bdiv) }

    | "<>"
        { BINOP (Bneq) }

    | "&&"
        { BINOP (Band) }

    | "||"
        { BINOP (Bor) }

    | "=="
        { BINOP (Beq) }

    | "<"
        { BINOP (Blt) }

    | "<="
        { BINOP (Ble) }

    | ">"
        { BINOP (Bgt) }

    | ">="
        { BINOP (Bge) }

    | "="
        { EQUAL }

    | "->"
        { ARROW }

    | "=>"
        { FAT_ARROW }
        
    | "("
        { LEFTPAR }

    | ")"
        { RIGHTPAR }

    | "{"
        { LEFTBRACE }

    | "}"
        { RIGHTBRACE }

    | ";"
        { SEMICOLON }
    
    | eof
        { EOF }

    | _
        { raise (Lexing_error ("erreur") ) }

and line_comment = parse
    | eol
        { new_line lexbuf; next_token lexbuf }

    | _
        { line_comment lexbuf }

and block_comment = parse
    | eol
        { new_line lexbuf; block_comment lexbuf }

    | "-}"
        { next_token lexbuf }
    
    | eof
        { raise (Lexing_error ("unterminated comment")) }

    | _
        { block_comment lexbuf }

and string = parse
    | eol | eof
        { raise (Lexing_error ("unterminated string")) }

    | '"'
        { let s = Buffer.contents string_buffer in
          Buffer.reset string_buffer;
          s }

    | "\\n"
        { Buffer.add_char string_buffer '\n';
          string lexbuf }

    | "\\\""
        { Buffer.add_char string_buffer '"';
          string lexbuf }

    | _ as c
        { Buffer.add_char string_buffer c;
          string lexbuf }

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
