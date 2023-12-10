type t = {
    (* The line's number with 1 being the first line. *)
    lineno : int;
    (* The column's number with 1 being the first column. *)
    colno : int;
}

(* A source range [begin, end). *)
type range = t * t

let dummy = 
    { 
        lineno = 0;
        colno = 0;
    }

let dummy_range = dummy, dummy

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
    Printf.eprintf "\x1b[1mFile \"%s\", line %d, characters %d-%d:\x1b[0m\n" file s.lineno s.colno e.colno