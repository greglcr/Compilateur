open Format
open Lexing
open Parser
open Lexer
open Ast
open Typedtree

let usage = "usage: ppurs [options] file.purs"

let lexing_only = ref false
let parse_only = ref false
let typing_only = ref false

let spec =
    [
        "--lexing-only", Arg.Set lexing_only, "  stop after lexing and print all tokens";
        "--parse-only", Arg.Set parse_only, "  stop after parsing";
        "--typing-only", Arg.Set typing_only, "  stop after typing";
    ]

let file =
    let file = ref None in
    let set_file s =
        if not (Filename.check_suffix s ".purs") then
            raise (Arg.Bad "no .purs extension");
        file := Some s
    in
    Arg.parse spec set_file usage;
    match !file with Some f -> f | None -> Arg.usage spec usage; exit 1
  
let report (b,e) =
    let l = b.pos_lnum in
    let fc = b.pos_cnum - b.pos_bol + 1 in
    let lc = e.pos_cnum - b.pos_bol + 1 in
    eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let show_tokens lb =
    let token = ref (Post_lexer.next_token lb) in
    while !token != EOF do
        print_endline (_menhir_print_token !token);
        token := Post_lexer.next_token lb
    done

let () = 
    let c = open_in file in
    let lb = Lexing.from_channel c in
    try
        if !lexing_only then (
            show_tokens lb
        ) else (
            let f = Parser.file Post_lexer.next_token lb in
            close_in c;
            if !parse_only then exit 0;
            let typed_f = Typer.file f in
            if !typing_only then exit 0;
        )
    with
    | Lexing_error msg ->
        let s = Location.lexeme_start lb in
        let e = Location.lexeme_end lb in
        Location.print file s e;
        eprintf "lexical error: %s@." msg;
        exit 1
    | Semantic_error s ->
        report (lexeme_start_p lb, lexeme_end_p lb);
        eprintf "semantic error: %s@." s;
        exit 1
    | Parser.Error ->
        report (lexeme_start_p lb, lexeme_end_p lb);
        eprintf "syntax error@.";
        exit 1
    | Typer.Error s ->
        report (lexeme_start_p lb, lexeme_end_p lb);
        eprintf "typing error: %s@." s;
        exit 1
