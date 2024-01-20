open Format
open Lexing
open Parser
open Lexer
open Ast
open TypedTree

let () = Printexc.record_backtrace true
let usage = "usage: ppurs [options] file.purs"
let lex_only = ref false
let parse_only = ref false
let type_only = ref false
let compile = ref false
let execute = ref false

let spec =
  Arg.align
    [
      ( "--lex-only",
        Arg.Set lex_only,
        "  stop after lexing and print all tokens" );
      ("--parse-only", Arg.Set parse_only, "  stop after parsing");
      ("--type-only", Arg.Set type_only, "  stop after typing");
      ("-c", Arg.Set compile, "  generate a binary executable file directly");
      ( "-e",
        Arg.Set execute,
        "  execute on-fly the compiled PetitPureScript code" );
    ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".purs") then
      raise (Arg.Bad "no .purs extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with
  | Some f -> f
  | None ->
      Arg.usage spec usage;
      exit 1

let report (b, e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let print_error (range_start, range_end) msg =
  if not (Location.is_dummy_range (range_start, range_end)) then
    Location.print file range_start range_end;
  eprintf "\n\x1b[1;31mError\x1b[0m: %s@." msg;
  exit 1

let print_error_with_hint (range_start, range_end) msg hint =
  if not (Location.is_dummy_range (range_start, range_end)) then
    Location.print file range_start range_end;
  eprintf "\n\x1b[1;31mError\x1b[0m: %s@." msg;
  eprintf "\x1b[3mNote: %s\x1b[0m@." hint;
  exit 1

let show_tokens lb =
  let token = ref (PostLexer.next_token lb) in
  while !token != EOF do
    print_endline (_menhir_print_token !token);
    token := PostLexer.next_token lb
  done

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    if !lex_only then show_tokens lb
    else
      let f = Parser.file PostLexer.next_token lb in
      close_in c;
      if !parse_only then exit 0;
      let typed_f = Typer.type_file f in
      if !type_only then exit 0;

      let file_without_ext = Filename.remove_extension file in

      if !compile then (
        if not (Execute.compile (file_without_ext ^ ".exe") typed_f) then exit 1)
      else if !execute then
        match Execute.execute typed_f with
        | None -> exit (-1)
        | Some exit_code -> exit exit_code
      else Execute.compile_asm (file_without_ext ^ ".s") typed_f
  with
  | Lexing_error msg ->
      let range_start = Location.lexeme_start lb in
      let range_end = Location.lexeme_end lb in
      print_error (range_start, range_end) msg
  | Semantic_error (range, msg) -> print_error range msg
  | Parser.Error ->
      let range_start = Location.lexeme_start lb in
      let range_end = Location.lexeme_end lb in
      print_error (range_start, range_end) "syntax error"
  | TyperCommon.TypingError (range, msg, None) -> print_error range msg
  | TyperCommon.TypingError (range, msg, Some hint) ->
      print_error_with_hint range msg hint
