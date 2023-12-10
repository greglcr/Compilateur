open Format
open Lexing
open Parser
open Lexer
open Ast
open Typedtree
open Typedprinter

let usage = "usage: ppurs [options] file.purs"

let lex_only = ref false
let parse_only = ref false
let type_only = ref false

let spec =
    [
        "--lex-only", Arg.Set lex_only, "  stop after lexing and print all tokens";
        "--parse-only", Arg.Set parse_only, "  stop after parsing";
        "--type-only", Arg.Set type_only, "  stop after typing";
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

let print_error (range_start, range_end) msg =
    Location.print file range_start range_end;
    eprintf "\x1b[1;31mError\x1b[0m: %s@." msg;
    exit 1

let print_error_with_hint (range_start, range_end) msg hint =
    Location.print file range_start range_end;
    eprintf "\x1b[1;31mError\x1b[0m: %s@." msg;
    eprintf "\x1b[3mNote: %s\x1b[0m@." hint;
    exit 1

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
        if !lex_only then (
            show_tokens lb
        ) else (
            let f = Parser.file Post_lexer.next_token lb in
            close_in c;
            if !parse_only then exit 0;
            let typed_f = Typer.file f in

            let ppf = Format.std_formatter in
            pp_list pp_decl ppf typed_f;
            Format.pp_print_newline ppf ();
            Format.pp_print_flush ppf ();

            if !type_only then exit 0;
        )
    with
    | Lexing_error msg ->
        let range_start = Location.lexeme_start lb in
        let range_end = Location.lexeme_end lb in
        print_error (range_start, range_end) msg
    | Semantic_error (range, msg) ->
        print_error range msg
    | Parser.Error ->
        let range_start = Location.lexeme_start lb in
        let range_end = Location.lexeme_end lb in
        print_error (range_start, range_end) "syntax error"
    | Typer.Error (range, msg, None) ->
        print_error range msg
    | Typer.Error (range, msg, Some hint) ->
        print_error_with_hint range msg hint;
    | Typer.UnificationFailure (t1, t2) ->
        pp_typ Format.std_formatter t1;
        Format.pp_print_newline Format.std_formatter ();
        pp_typ Format.std_formatter t2;
        print_error (Location.dummy, Location.dummy) "unification error";
