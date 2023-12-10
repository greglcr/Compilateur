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

let file_lines filename =
    let lines = ref [] in
    let chan = open_in filename in
    try
        while true; do
        lines := input_line chan :: !lines
        done; !lines
    with End_of_file ->
        close_in chan;
        List.rev !lines

let print (file : string) (s : t) (e : t) =
    let lines = file_lines file in
    let line = List.nth_opt lines (s.lineno - 1) in
    Printf.eprintf "\x1b[1mFile \"%s\", line %d, characters %d-%d:\x1b[0m\n" file s.lineno s.colno e.colno;
    match line with
        | Some (line) -> 
            let line_length = String.length line in
            let underline = Buffer.create line_length in
            
            for i = 0 to s.colno - 2 do
                Buffer.add_char underline ' '
            done;

            let underline_length = 
                if s.lineno = e.lineno then e.colno - s.colno - 1
                else line_length - s.colno
            in
            for i = 0 to underline_length do
                Buffer.add_char underline '^'
            done;

            Printf.eprintf " %4d | %s\n        %s\n" s.lineno line (Buffer.contents underline)

        | None -> ()