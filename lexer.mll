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

                        ("(", LEFTPAR);
                        (")", RIGHTPAR);
                        ("{", LEFTBRACE);
                        ("}", RIGHTBRACE)
                    ]
                  );
        fun s -> try Hashtbl.find linkMap s with Not_found -> IDENT s
}

let digit = ['0'-'9']
let lower = ['a'-'z' '_']
let upper = ['A'-'Z']
let mathexpr = "+" | "-" | "*" | "/" | "<>" | "&&" | "||" | "==" | "<" | "<=" | ">" | ">="
let sep = "(" | ")" | "{" | "}"
let other = lower | upper | digit | "'"
let lident = lower other*  
let uident = upper (other | '.')*

rule next_token = parse
    | '\n' '\r' '\t' ' '
        { next_token lexbuf }

    | lower* | mathexpr | sep as id
        { find_valeur id }

    | digit+ as n
        { CST (Cint (int_of_string n) ) }
    
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

    | _
        { comment lexbuf }

{

    let print_lexeme fileName =
        let c = open_in fileName in
        let lb = from_channel c in
        let rec liste_lexemes curTok = match curTok with
            | EOF -> print_string " EOF\n"
            | _ -> print_string (Lexing.lexeme lb); print_string " "; (liste_lexemes (next_token lb)) in
        (liste_lexemes (next_token lb));;

}



    
    
