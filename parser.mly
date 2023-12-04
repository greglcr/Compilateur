%{

    open Ast
    open Format

%}

%token <Ast.constant> CST (* Lexème qui va retourner une constante. Ici on peut directement renvoyer le type
                            correspondant aux constantes de la syntaxe abstraite car il n'y a qu'une seule
                            possibilité *)

%token CASE CLASS DATA DO ELSE FORALL IF IMPORT IN INSTANCE LET MODULE OF THEN WHERE
%token <string> LIDENT UIDENT
%token LPAR RPAR
%token LBRACE RBRACE
%token EOF
%token ARROW FAT_ARROW
%token SEMI COLON_COLON DOT COMMA

%token EQ EQ_EQ SLASH_EQ LESS LESS_EQ GREATER GREATER_EQ
%token PLUS MINUS STAR SLASH LESS_GREATER AMP_AMP PIPE_PIPE

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
        i = import*
        decls = separated_nonempty_list(SEMI, decl)
      RBRACE EOF
        { Fprogramm decls }
;

import:
    | IMPORT name = UIDENT SEMI
        {}
;

decl:
    | d = defn
        { DECLdefn (d) }

    | td = tdecl
        { DECLtdecl (td) }

    // | DATA u1 = UIDENT lli = LIDENT* EQ WHERE lpair = uident_latype+
    //     { DECLdata (u1, lli, lpair) }

    // | CLASS u = UIDENT lli = LIDENT* WHERE LBRACE ltde = tdecl* SEMI RBRACE
    //     { DECLclass (u, lli, ltde) }

    // | INSTANCE i = instance WHERE LBRACE ld = tdecl* SEMI RBRACE
    //     { DECLinstance (i, ld) }
;

uident_latype:
    | u = UIDENT latp = atype*
        { (u, latp) }

defn:
    | lid = LIDENT p = patarg* EQ e = expr
        { DEF (lid, p, e) }
;

tdecl:
    | li = LIDENT COLON_COLON LPAR lli = forall RPAR
      ld = ntype_fatarrow* lt = tp_arrow* t = typ
        { TDECL (li, lli, ld, lt, t) }
;

forall:
    | FORALL vars = LIDENT+ DOT
        { vars }

    |
        { [] }
;

ntype_fatarrow:
    | nt = ntype FAT_ARROW
        { nt }
;

tp_arrow:
    | t = typ ARROW
        { t }
;

ntype:
    | name = UIDENT args = atype*
        { Tsymbol (name, args) }
;

atype:
    | name = LIDENT
        { Tvar (name) }

    | name = UIDENT
        { Tsymbol (name, []) }

    | LPAR t = typ RPAR
        { t }
;

typ:
    | t = atype
    | t = ntype
        { t }
;

instance:
    | nt = ntype
        { INSTntp (nt) }

    | nt1 = ntype FAT_ARROW nt2 = ntype
        { INSTntpc (nt1, nt2) }

    | LPAR lnt = separated_nonempty_list(COMMA, ntype) RPAR FAT_ARROW nt = ntype
        { INSTntpcc (lnt, nt) }

patarg:
    | c = constant
        { Pconst (c) }

    | name = LIDENT
        { Pvar (name) }

    | name = UIDENT
        { Psymbol (name, []) }

    | LPAR p = pattern RPAR
        { p }
;

pattern:
    | p = patarg
        { p }

    | name = UIDENT args = patarg+
        { Psymbol (name, args) }

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
