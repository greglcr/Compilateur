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

let whitespace = ['\t' ' ']
let nonzero_digit = ['1'-'9']
let digit = ['0'-'9']
let integer = nonzero_digit digit* | '0'
let lower = ['a'-'z' '_']
let upper = ['A'-'Z']
let other = lower | upper | digit | "'"
let lident = lower other*
let uident = upper (other | '.')*
let eol = '\n' | '\r' | "\r\n"

rule next_token = parse
    | whitespace
        { next_token lexbuf }

    | eol
        { new_line lexbuf; next_token lexbuf }

    | "--"
        { line_comment lexbuf}

    | "{-"
        { block_comment lexbuf }

    | integer as s
        { try (CST (Cint (int_of_string s)))
          with _ -> raise (Lexing_error ("constant too large: " ^ s)) }

    | lident as l
        { resolve_keyword l }

    | uident as u
        { UIDENT (u) }

    | '"'
        { CST (Cstring (string lexbuf)) }

    | "+"
        { PLUS }

    | "-"
        { MINUS }

    | "*"
        { STAR }

    | "/"
        { SLASH }

    | "<>"
        { LESS_GREATER }

    | "&&"
        { AMP_AMP }

    | "||"
        { PIPE_PIPE }

    | "=="
        { EQ_EQ }

    | "/="
        { SLASH_EQ }

    | "<"
        { LESS }

    | "<="
        { LESS_EQ }

    | ">"
        { GREATER }

    | ">="
        { GREATER_EQ }

    | "="
        { EQ }

    | "->"
        { ARROW }

    | "=>"
        { FAT_ARROW }

    | "("
        { LPAR }

    | ")"
        { RPAR }

    | "{"
        { LBRACE }

    | "}"
        { RBRACE }

    | ";"
        { SEMI }

    | "::"
        { COLON_COLON }

    | ","
        { COMMA }

    | "."
        { DOT }

    | "|"
        { PIPE }

    | eof
        { EOF }

    | _ as c
        { raise (Lexing_error ("illegal character: " ^ String.make 1 c )) }

and line_comment = parse
    | eol
        { new_line lexbuf; next_token lexbuf }

    | eof
        { EOF }

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

    | "\\\\"
        { Buffer.add_char string_buffer '\\';
          string lexbuf }

    | "\\\""
        { Buffer.add_char string_buffer '"';
          string lexbuf }

    | '\\'
        { ignore_whitespace_in_string lexbuf }

    | _ as c
        { Buffer.add_char string_buffer c;
          string lexbuf }

and ignore_whitespace_in_string = parse
    | eof
        { raise (Lexing_error ("unterminated string")) }

    | whitespace+
        { ignore_whitespace_in_string lexbuf }

    | eol
        { new_line lexbuf; ignore_whitespace_in_string lexbuf }

    | '\\'
        { string lexbuf }

    | _ as c
        { raise (Lexing_error ("unexpected character:" ^ String.make 1 c)) }

{

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | AMP_AMP ->
        "AMP_AMP"
    | ARROW ->
        "ARROW"
    | CASE ->
        "CASE"
    | CLASS ->
        "CLASS"
    | COLON_COLON ->
        "COLON_COLON"
    | COMMA ->
        "COMMA"
    | CST _ ->
        "CST"
    | DATA ->
        "DATA"
    | DO ->
        "DO"
    | DOT ->
        "DOT"
    | ELSE ->
        "ELSE"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | EQ_EQ ->
        "EQ_EQ"
    | FAT_ARROW ->
        "FAT_ARROW"
    | FORALL ->
        "FORALL"
    | GREATER ->
        "GREATER"
    | GREATER_EQ ->
        "GREATER_EQ"
    | IF ->
        "IF"
    | IMPORT ->
        "IMPORT"
    | IN ->
        "IN"
    | INSTANCE ->
        "INSTANCE"
    | LBRACE ->
        "LBRACE"
    | LPAR ->
        "LPAR"
    | LESS ->
        "LESS"
    | LESS_EQ ->
        "LESS_EQ"
    | LESS_GREATER ->
        "LESS_GREATER"
    | LET ->
        "LET"
    | LIDENT _ ->
        "LIDENT"
    | MINUS ->
        "MINUS"
    | MODULE ->
        "MODULE"
    | OF ->
        "OF"
    | PIPE ->
        "PIPE"
    | PIPE_PIPE ->
        "PIPE_PIPE"
    | PLUS ->
        "PLUS"
    | RBRACE ->
        "RBRACE"
    | RPAR ->
        "RPAR"
    | SEMI ->
        "SEMI"
    | SLASH ->
        "SLASH"
    | SLASH_EQ ->
        "SLASH_EQ"
    | STAR ->
        "STAR"
    | THEN ->
        "THEN"
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
