open Format
open Lexing
open Parser
open Lexer
open Ast


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
    
    and print_patarg = function
        | PATARconst c -> Printf.printf "PATARconst (";
                          print_const c;
                          Printf.printf ")";
        | _ -> raise (Print_error "patarg")

    and print_expr = function
        | Ebinop(b, e1, e2) -> Printf.printf "Ebinop ("; print_binop b; print_expr e1;
                                print_expr e2; Printf.printf ") "
        | Eatom (a) -> Printf.printf "Eatom ("; print_atom a; Printf.printf ")";
        | _ -> raise (Print_error "expr")
    
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
                        (Bmod, "Bmod");
                        (Beq, "Beq");
                        (Bneq, "Bneq");
                        (Blt, "Blt");
                        (Ble, "Ble");
                        (Bgt, "Bgt");
                        (Bge, "Bge");
                        (Band, "Band");
                        (Bor, "Bor")
                    ]
                )
        ;
        try Printf.printf "%s " (Hashtbl.find linkMap sa)
        with Not_found -> raise (Print_error "binop")

    and print_lident l = Printf.printf "lident (%s)" l

    and print_uident u = Printf.printf "uident (%s)" u

    and print_ident i = Printf.printf "ident (%s)" i in

    print_f sa
    
let () = Lexer.print_lexeme "test1.purs"

let () = 
    let f = "test1.purs" in
    let c = open_in f in
    let lb = Lexing.from_channel c in
    let f = Parser.file Lexer.next_token lb in
    print_file f;;

