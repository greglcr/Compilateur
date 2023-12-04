%{

    open Ast
    open Format

    module Imports = Set.Make(String)

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

atype:
    | name = LIDENT
        { Tvar (name) }

    | name = UIDENT
        { Tsymbol (name, []) }

    | LPAR t = typ RPAR
        { t }
;

defn:
    | lid = LIDENT p = patarg* EQ e = expr
        { DEF (lid, p, e) }
;

tdecl:
    | li = LIDENT COLON_COLON lli = forall
      separated_nonempty_list(ARROW, typ)
        { TDECL (li, lli, [], [], Tvar "") }
;

forall:
    | FORALL vars = LIDENT+ DOT
        { vars }

    |
        { [] }
;

ntype:
    | name = UIDENT args = atype*
        { Tsymbol (name, args) }
;

typ:
    | name = LIDENT
        { Tvar (name) }

    | name = UIDENT args = typ*
        { Tsymbol (name, args) }

    | LPAR t = typ RPAR
        { t }
;

instance:
    | nt = ntype
        { INSTntp (nt) }

    | nt1 = ntype FAT_ARROW nt2 = ntype
        { INSTntpc (nt1, nt2) }

    | LPAR lnt = separated_nonempty_list(COMMA, ntype) RPAR FAT_ARROW nt = ntype
        { INSTntpcc (lnt, nt) }
;

patarg:
    | c = constant
        { Pconst (c) }

    | name = LIDENT
        { Pvar (name) }

    | name = UIDENT
        { Papp (name, []) }

    | LPAR p = pattern RPAR
        { p }
;

pattern:
    | p = patarg
        { p }

    | name = UIDENT args = patarg+
        { Papp (name, args) }

constant:
    | c = CST
        { c }
;

atom:
    | c = constant
        { Econst (c) }

    | name = LIDENT
        { Eapp (name, []) }

    | name = UIDENT
        { Eapp (name, []) }

    | LPAR e = expr RPAR
        { e }
;

expr:
    | a = atom
        { a }

    | name = LIDENT args = atom+
    | name = UIDENT args = atom+
        { Eapp (name, args) }

    | MINUS e = expr %prec UNARY_MINUS
        { Ebinop (Bsub, Econst (Cint 0), e) }

    | lhs = expr op = binop rhs = expr
        { Ebinop (op, lhs, rhs) }

    | IF cond = expr THEN then_ = expr ELSE else_ = expr
        { Eif (cond, then_, else_) }

    | DO LBRACE body = separated_nonempty_list(SEMI, expr) RBRACE
        { Edo (body) }

    | LET LBRACE bindings = separated_nonempty_list(SEMI, binding) RBRACE IN e = expr
        { Elet (bindings, e) }

    | CASE cond = expr OF LBRACE lbranch = separated_nonempty_list(SEMI, branch) RBRACE
        { Ecase (cond, lbranch) }
;

binding:
    | name = LIDENT EQ e = expr
        { Baffect (name, e) }

branch:
    | p = pattern ARROW e = expr
        { Barrow (p, e) }

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
