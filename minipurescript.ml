open Format
open Lexing
open Parser
open Lexer
open Ast

let usage = "usage: ppurs [options] file.purs"

let lexing_only = ref false
let parse_only = ref false

let spec =
    [
        "--lexing-only", Arg.Set lexing_only, "  stop after lexing and print all tokens";
        "--parse-only", Arg.Set parse_only, "  stop after parsing";
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
    
exception Print_error of string

let rec print_file sa = 

    let rec print_f sa = 
            Printf.printf "Fprogramm (";
            let Fprogramm (l) = sa in
            List.iter (fun x -> print_decl x)
                        l;  
            Printf.printf ")";

        and print_decl = function
            | DECLdefn d -> Printf.printf "DECLdefn ("; print_defn d; Printf.printf ")"
            | _ -> raise (Print_error "decl")

        and print_defn = function
            | DEF (lid, lpat, e) -> Printf.printf "DEF (";
                                    print_lident lid;
                                    List.iter (fun x -> print_patarg x) lpat;
                                    print_expr e;
                                    Printf.printf ")";

        and print_tdecl = function
            | _ -> raise (Print_error "tdecl")

        and print_ntype = function
            | _ -> raise (Print_error "ntype")
        
        and print_atype = function
            | _-> raise (Print_error "atype")
        
        and print_tp = function
            | _ -> raise (Print_error "tp")
        
        and print_instance = function
            | _ -> raise (Print_error "instance")

        and print_patarg = function
            | PATARconst c -> Printf.printf "PATARconst (";
                            print_const c;
                            Printf.printf ")";
            | _ -> raise (Print_error "patarg")

        and print_pattern = function
            | PATERpatar (p) -> Printf.printf "PATERpatar ("; print_patarg p;
                                Printf.printf ")";
            | PATERjspquelnom (u, lp) -> Printf.printf "PATERjspquelnom ("; print_uident u;
                                        List.iter (fun x -> print_patarg x) lp;
                                        Printf.printf ")";

        and print_expr = function
            | Ebinop(b, e1, e2) -> Printf.printf "Ebinop ("; print_binop b; print_expr e1;
                                    print_expr e2; Printf.printf ") "
            | Eatom (a) -> Printf.printf "Eatom ("; print_atom a; Printf.printf ")";
            | _ -> raise (Print_error "expr")

        and print_branch = function
            | Barrow (p, e) -> Printf.printf "Barrow ("; print_pattern p; print_expr e;
                                Printf.printf ")"; 

        and print_atom = function
            | Aconst (c) -> Printf.printf "Aconst ("; print_const c; Printf.printf ")";
            | Alident (l) -> Printf.printf "Alident ("; print_lident l; Printf.printf ")";
            | Auident (u) -> Printf.printf "Auident ("; print_uident u; Printf.printf ")";
            | Aexpr (e) -> Printf.printf "Aexpr ("; print_expr e; Printf.printf ")";
            | _ -> raise (Print_error "atom")

        and print_const = function
            | Cbool(false) -> Printf.printf "Cbool (false) "
            | Cbool(true) -> Printf.printf "Cbool (true) "
            | Cint (c) -> Printf.printf "Cint (%d) " c
            | Cstring (s) -> Printf.printf "Cstring (%s) " s 

        and print_binop sa =
            let linkMap = Hashtbl.create 17 in
            List.iter (fun (bin, binString) -> Hashtbl.add linkMap bin binString)
                    (
                        [
                            (Badd, "Badd");
                            (Bsub, "Bsub");
                            (Bmul, "Bmul");
                            (Bdiv, "Bdiv");
                            (Beq, "Beq");
                            (Bneq, "Bneq");
                            (Blt, "Blt");
                            (Ble, "Ble");
                            (Bgt, "Bgt");
                            (Bge, "Bge");
                            (Band, "Band");
                            (Bor, "Bor");
                            (Bconcat, "Bconcat");
                        ]
                    )
            ;
            try Printf.printf "%s " (Hashtbl.find linkMap sa)
            with Not_found -> raise (Print_error "binop")

        and print_lident l = Printf.printf "lident (%s)" l

        and print_uident u = Printf.printf "uident (%s)" u

        and print_ident i = Printf.printf "ident (%s)" i in

    print_f sa

let () = 
    let c = open_in file in
    let lb = Lexing.from_channel c in
    try
        let f = Parser.file Post_lexer.next_token lb in
        close_in c;
        if !parse_only then exit 0;
    with
    | Lexer.Lexing_error s ->
        report (lexeme_start_p lb, lexeme_end_p lb);
        eprintf "lexical error: %s@." s;
        exit 1
    | Parser.Error ->
        report (lexeme_start_p lb, lexeme_end_p lb);
        eprintf "syntax error@.";
        exit 1
