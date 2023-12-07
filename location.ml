type t = {
    lineno : int;
    colno : int;
}

let dummy = 
    { 
        lineno = 0;
        colno = 0;
    }

let from_lexing_position (p : Lexing.position) =
    {
        lineno = p.pos_lnum;
        colno = p.pos_cnum - p.pos_bol + 1;
    }

let lexeme_start (lexbuf : Lexing.lexbuf) =
    let p = Lexing.lexeme_start_p lexbuf in
    from_lexing_position p

let lexeme_end (lexbuf : Lexing.lexbuf) =
    let p = Lexing.lexeme_end_p lexbuf in
    from_lexing_position p

let print (file : string) (s : t) (e : t) =
    Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n" file s.lineno s.colno e.colno