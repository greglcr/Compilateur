%{

    open Ast
    open Format

    module Imports = Set.Make(String)

    let create_lexeme_node (s, e) node =
        {
            start_loc = Location.from_lexing_position s;
            end_loc = Location.from_lexing_position e;
            node
        }
%}

(* Keywords *)
%token CASE CLASS DATA DO ELSE FORALL IF IMPORT IN INSTANCE LET MODULE OF THEN WHERE

(* Identifiers and constants *)
%token <string> LIDENT UIDENT
%token <Ast.constant> CST

(* Punctuation *)
%token EOF
%token LPAR RPAR LBRACE RBRACE
%token ARROW FAT_ARROW
%token SEMI COLON_COLON DOT COMMA

(* Operator tokens *)
%token EQ EQ_EQ SLASH_EQ LESS LESS_EQ GREATER GREATER_EQ
%token PLUS MINUS STAR SLASH LESS_GREATER AMP_AMP PIPE_PIPE PIPE

(* Associativities and precedences *)
%nonassoc IN ELSE
%left PIPE_PIPE
%left AMP_AMP
%nonassoc EQ_EQ SLASH_EQ LESS LESS_EQ GREATER GREATER_EQ
%left PLUS MINUS LESS_GREATER
%left STAR SLASH
%nonassoc UNARY_MINUS

(*
 Seules les fonctions qui sont placées après un start seront copiées dans le fichier .mli,
 donc seules celles là pourront être appelées en dehors de ce fichier.
*)
%start file
%type <Ast.file> file

%%


file:
    | MODULE name = UIDENT WHERE LBRACE 
        imports = imports
        decls = separated_nonempty_list(SEMI, decl) 
      RBRACE EOF
        { 
            if not (Imports.mem "Prelude" imports) then
                raise (Semantic_error "missing 'import Prelude'");
            if not (Imports.mem "Effect" imports) then
                raise (Semantic_error "missing 'import Effect'");
            if not (Imports.mem "Effect.Console" imports) then
                raise (Semantic_error "missing 'import Effect.Console'");

            if name <> "Main" then
                raise (Semantic_error "expected 'Main' as module name");

            Fprogram (decls) 
        }
;

imports:
    | IMPORT name = UIDENT SEMI
        {
            (* For PureScript, duplicating imports is not an error. *)
            Imports.singleton name
        }

    | i = imports IMPORT name = UIDENT SEMI
        {
            Imports.add name i
        }
;

decl:
    | d = defn
        { d }

    | td = tdecl
        { td }

    | d = decl_internal
        { create_lexeme_node $loc(d) d }
;

decl_internal:
    | DATA name = uident lli = lident* EQ nt = separated_nonempty_list(PIPE, typ)
         { Pdecl_data (name, lli, nt) }

    | CLASS name = uident lli = lident* WHERE LBRACE ltde = separated_list(SEMI, tdecl) RBRACE
        { Pdecl_class (name, lli, ltde) }

    | INSTANCE i = instance WHERE LBRACE ld = separated_list(SEMI, defn) RBRACE
         { Pdecl_instance (i, ld) }
;

defn:
    | name = LIDENT p = patarg* EQ e = expr
        { create_lexeme_node ($startpos(name), $endpos(e)) (Pdecl_func (create_lexeme_node $loc(name) name, p, e)) }
;

tdecl:
    | name = lident COLON_COLON gen_vars = forall
      typs = separated_nonempty_list(ARROW, typ)
        {
            let rev_typs = List.rev typs in
            let last_typ = List.hd (List.rev typs) in
            create_lexeme_node ($startpos(name), $endpos(typs)) (Pdecl_func_signature 
                (name, gen_vars, [], List.rev (List.tl rev_typs), last_typ))
        }
;

forall:
    | FORALL vars = lident+ DOT
        { vars }

    |
        { [] }
;

atype:
    | name = LIDENT
        { create_lexeme_node $loc(name) (Ptyp_variable (name)) }

    | name = UIDENT
        { create_lexeme_node $loc(name)
            (Ptyp_apply (create_lexeme_node $loc(name) name, [])) }

    | LPAR t = typ RPAR
        { t }
;

ntype:
    | name = uident args = atype*
        { create_lexeme_node 
            ($startpos(name), $endpos(args))
            (Ptyp_apply (name, args)) }
;

typ:
    | name = LIDENT
        { create_lexeme_node $loc(name) (Ptyp_variable (name)) }

    | name = uident args = typ*
        { create_lexeme_node 
            ($startpos(name), $endpos(args))
            (Ptyp_apply (name, args)) }

    | LPAR t = typ RPAR
        { t }
;

instance:
    | nt = ntype
        { [ nt ], None }

    | nt1 = ntype FAT_ARROW nt2 = ntype
        { ([nt1], Some nt2) }

    | LPAR lnt = separated_nonempty_list(COMMA, ntype) RPAR FAT_ARROW nt = ntype
        { (lnt, Some nt) }
;

patarg:
    | c = CST
        { create_lexeme_node $loc(c) (Ppattern_constant (c)) }

    | name = LIDENT
        { create_lexeme_node $loc(name) (Ppattern_variable (name)) }

    | name = UIDENT
        { create_lexeme_node $loc(name) (Ppattern_apply (create_lexeme_node $loc(name) name, [])) }

    | LPAR p = pattern RPAR
        { p }
;

pattern:
    | p = patarg
        { p }

    | name = uident args = patarg+
        { create_lexeme_node 
            ($startpos(name), $endpos(args))
            (Ppattern_apply (name, args)) }

atom:
    | a = atom_internal
        { create_lexeme_node $loc(a) a }

(* atom without location *)
atom_internal:
    | c = CST
        { Pexpr_constant (c) }

    | name = lident
        { Pexpr_apply (name, []) }

    | name = uident
        { Pexpr_apply (name, []) }

    | LPAR e = expr_internal RPAR
        { e }
;

expr:
    | e = expr_internal
        { create_lexeme_node $loc(e) e }

(* expr without location *)
expr_internal:
    | a = atom_internal
        { a }

    | name = lident args = atom+
    | name = uident args = atom+
        { Pexpr_apply (name, args) }

    | MINUS e = expr %prec UNARY_MINUS
        { Pexpr_neg (e) }

    | lhs = expr op = binop rhs = expr
        { Pexpr_binary (create_lexeme_node $loc(op) op, lhs, rhs) }

    | IF cond = expr THEN then_ = expr ELSE else_ = expr
        { Pexpr_if (cond, then_, else_) }

    | DO LBRACE body = separated_nonempty_list(SEMI, expr) RBRACE
        { Pexpr_do (body) }

    | LET LBRACE bindings = separated_nonempty_list(SEMI, binding) RBRACE IN e = expr
        { Pexpr_let (bindings, e) }

    | CASE cond = expr OF LBRACE lbranch = separated_nonempty_list(SEMI, branch) RBRACE
        { Pexpr_case (cond, lbranch) }
;

binding:
    | name = lident EQ e = expr
        { (name, e) }

branch:
    | p = pattern ARROW e = expr
        { (p, e) }

lident:
    | ident = LIDENT
        { create_lexeme_node $loc(ident) ident }
;

uident:
    | ident = UIDENT
        { create_lexeme_node $loc(ident) ident }
;

%inline binop:
| EQ_EQ { Beq }
| SLASH_EQ { Bneq }
| LESS { Blt }
| LESS_EQ { Ble }
| GREATER { Bgt }
| GREATER_EQ { Bge }
| PLUS { Badd }
| MINUS { Bsub }
| STAR { Bmul }
| SLASH { Bdiv }
| LESS_GREATER { Bconcat }
| AMP_AMP { Band }
| PIPE_PIPE { Bor }
;
