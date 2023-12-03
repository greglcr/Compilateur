%{

    open Ast
    open Format

%}

%token <Ast.constant> CST (* Lexème qui va retourner une constante. Ici on peut directement renvoyer le type
                            correspondant aux constantes de la syntaxe abstraite car il n'y a qu'une seule
                            possibilité *)

%token CASE CLASS DATA DO ELSE FORALL IF IMPORT IN INSTANCE LET MODULE OF THEN WHERE
%token <string> LIDENT UIDENT
%token LEFTPAR RIGHTPAR
%token LEFTBRACE RIGHTBRACE
%token EOF
%token ARROW FAT_ARROW
%token SEMICOLON COLON_COLON QUESTION DOT COMMA

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
    | MODULE name = UIDENT WHERE LEFTBRACE i = import* d = separated_nonempty_list(SEMICOLON, decl) RIGHTBRACE EOF
        { Fprogramm d }
;

import:
    | IMPORT name = UIDENT SEMICOLON
        {}
;

decl:
    | d = defn
        { DECLdefn (d) }
    
    | td = tdecl
        { DECLtdecl (td) }

    | DATA u1 = UIDENT lli = LIDENT* EQ WHERE lpair = uident_latype+
        { DECLdata (u1, lli, lpair) }

    | CLASS u = UIDENT lli = LIDENT* WHERE LEFTBRACE ltde = tdecl* SEMICOLON RIGHTBRACE
        { DECLclass (u, lli, ltde) }
    
    | INSTANCE i = instance WHERE LEFTBRACE ld = tdecl* SEMICOLON RIGHTBRACE
        { DECLinstance (i, ld) }
;

uident_latype:
    | u = UIDENT latp = atype*
        { (u, latp) }

defn:
    | lid = LIDENT p = patarg* EQ e = expr
        { DEF (lid, p, e) }
;

tdecl:
    | li = LIDENT COLON_COLON LEFTPAR lli = forall RIGHTPAR
      ld = ntype_fatarrow* lt = tp_arrow* t = tp
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
    | t = tp ARROW
        { t }
;

ntype:
    | u = UIDENT la = atype*
        { NTP (u, la) }
;

atype:
    | l = LIDENT
        { ATl (l) }
    
    | u = UIDENT
        { ATu (u) }
    
    | LEFTPAR t = tp RIGHTPAR
        { ATt (t) }
;

tp:
    | at = atype
        { TPa (at) }
    
    | nt = ntype
        { TPn (nt) }
;

instance:
    | nt = ntype
        { INSTntp (nt) }
    
    | nt1 = ntype FAT_ARROW nt2 = ntype
        { INSTntpc (nt1, nt2) }
    
    | LEFTPAR lnt = separated_nonempty_list(COMMA, ntype) RIGHTPAR FAT_ARROW nt = ntype
        { INSTntpcc (lnt, nt) }

patarg:
    | c = constant
        { PATARconst (c) }
    
    | l = LIDENT
        { PATARlid (l) }
    
    | u = UIDENT
        { PATARuid (u) }
    
    | LEFTPAR p = pattern RIGHTPAR
        { PATARpat (p) }
;

pattern:
    | p = patarg
        { PATERpatar (p) }
    
    | u = UIDENT lp = patarg+
        { PATERjspquelnom (u, lp) }    

constant:
    | c = CST
        { c }
;

atom:
    | c = constant
        { Aconst (c) }

    | l = LIDENT
        { Alident (l) }
    
    | u = UIDENT
        { Auident (u) }
    
    | LEFTPAR e = expr RIGHTPAR
        { Aexpr (e) }
;

expr:
    | a = atom
        { Eatom (a) }
    
    | name = LIDENT args = atom+
        { Efonct (name, args) }
    
    | name = UIDENT args = atom+
        { Emodule (name, args) }

    | MINUS e = expr %prec UNARY_MINUS
        { Ebinop (Bsub, Eatom (Aconst (Cint 0)), e) }
    
    | lhs = expr op = binop rhs = expr
        { Ebinop (op, lhs, rhs) } 

    | IF cond = expr THEN then_ = expr ELSE else_ = expr
        { Econd (cond, then_, else_) }
    
    | DO LEFTBRACE body = separated_nonempty_list(SEMICOLON, expr) RIGHTBRACE
        { Edo (body) }
    
    | LET LEFTBRACE bindings = separated_nonempty_list(SEMICOLON, binding) RIGHTBRACE IN e = expr
        { Eaffect (bindings, e) }
    
     | CASE cond_ = expr OF LEFTBRACE lbranch = separated_nonempty_list(SEMICOLON, branch) RIGHTBRACE
        { Ecase (cond_, lbranch) }
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
 