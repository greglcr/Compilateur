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
        { DECLdefn (d) }

    | td = tdecl
        { DECLtdecl (td) }

    | DATA u1 = UIDENT lli = LIDENT* EQ nt = separated_nonempty_list(PIPE, typ)
         { DECLdata (u1, lli, nt) }

    | CLASS u = UIDENT lli = LIDENT* WHERE LBRACE ltde = separated_list(SEMI, tdecl) RBRACE
        { DECLclass (u, lli, ltde) }

    | INSTANCE i = instance WHERE LBRACE ld = separated_list(SEMI, defn) RBRACE
         { DECLinstance (i, ld) }
;

defn:
    | lid = LIDENT p = patarg* EQ e = expr
        { DEF (lid, p, e) }
;

tdecl:
    | li = LIDENT COLON_COLON lli = forall
      separated_nonempty_list(ARROW, typ)
        { TDECL (li, lli, [], [], Ptyp_variable "") }
;

forall:
    | FORALL vars = LIDENT+ DOT
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
    | name = UIDENT args = atype*
        { create_lexeme_node 
            ($startpos(name), $endpos(args))
            (Ptyp_apply (create_lexeme_node $loc(name) name, args)) }
;

typ:
    | name = LIDENT
        { create_lexeme_node $loc(name) (Ptyp_variable (name)) }

    | name = UIDENT args = typ*
        { create_lexeme_node 
            ($startpos(name), $endpos(args))
            (Ptyp_apply (create_lexeme_node $loc(name) name, args)) }

    | LPAR t = typ RPAR
        { t }
;

instance:
    | nt = ntype
        { Pinstance (nt) }

    | nt1 = ntype FAT_ARROW nt2 = ntype
        { Pinstance_dep ([nt1], nt2) }

    | LPAR lnt = separated_nonempty_list(COMMA, ntype) RPAR FAT_ARROW nt = ntype
        { Pinstance_dep (lnt, nt) }
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

    | name = UIDENT args = patarg+
        { create_lexeme_node 
            ($startpos(name), $endpos(args))
            (Ppattern_apply (create_lexeme_node $loc(name) name, args)) }

atom:
    | a = atom_internal
        { create_lexeme_node $loc(a) a }

(* atom without location *)
atom_internal:
    | c = CST
        { Pexpr_constant (c) }

    | name = LIDENT
        { Pexpr_apply (create_lexeme_node $loc(name) name, []) }

    | name = UIDENT
        { Pexpr_apply (create_lexeme_node $loc(name) name, []) }

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

    | name = LIDENT args = atom+
    | name = UIDENT args = atom+
        { Pexpr_apply (create_lexeme_node $loc(name) name, args) }

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
    | name = LIDENT EQ e = expr
        { (create_lexeme_node $loc(name) name, e) }

branch:
    | p = pattern ARROW e = expr
        { (p, e) }

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
