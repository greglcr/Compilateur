%{

    open Ast
    open Format

%}

%token <Ast.constant> CST (* Lexème qui va retourner une constante. Ici on peut directement renvoyer le type
                            correspondant aux constantes de la syntaxe abstraite car il n'y a qu'une seule
                            possibilité *)

%token <Ast.binop> BINOP (* Pareil *)

%token CASE CLASS DATA DO ELSE FALSE FORALL IF IMPORT IN INSTANCE LET MODULE OF THEN TRUE WHERE
%token <string> LIDENT UIDENT
%token LEFTPAR RIGHTPAR
%token LEFTBRACE RIGHTBRACE
%token EOF
%token <string> IDENT
%token ARROW FAT_ARROW
%token SEMICOLON COLON_COLON QUESTION DOT

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
    | MODULE name = UIDENT WHERE LEFTBRACE i = import* d = nonempty_list(decl) SEMICOLON RIGHTBRACE EOF
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

    | DATA u1 = uident lli = list(lident) EQ WHERE lpair = nonempty_list(uident_latype)
        { DECLdata (u1, lli, lpair) }

    | CLASS u = uident lli = list(lident) WHERE LEFTBRACE ltde = list(tdecl) SEMICOLON RIGHTBRACE
        { DECLclass (u, lli, ltde) }
    
    | INSTANCE i = instance WHERE LEFTBRACE ld = list(tdecl) SEMICOLON RIGHTBRACE
        { DECLinstance (i, ld) }
;

uident_latype:
    | u = uident latp = list(atype)
        { (u, latp) }

defn:
    | lid = lident p = list (patarg) EQ e = expr
        { DEF (lid, p, e) }
;

tdecl:
    | li = lident COLON_COLON LEFTPAR FORALL lli = nonempty_list(lident) DOT RIGHTPAR QUESTION
      ld = list(ntype_fatarrow) lt = list(tp_arrow) t = tp
        { TDECL (li, lli, ld, lt, t) }
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
    | u = uident la = nonempty_list(atype)
        { NTP (u, la) }
;

atype:
    | l = lident
        { ATl (l) }
    
    | u = uident
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
    
    | LEFTPAR lnt = list(ntype) RIGHTPAR FAT_ARROW nt = ntype
        { INSTntpcc (lnt, nt) }

patarg:
    | c = constant
        { PATARconst (c) }
    
    | l = lident
        { PATARlid (l) }
    
    | u = uident
        { PATARuid (u) }
    
    | LEFTPAR p = pattern RIGHTPAR
        { PATARpat (p) }
;

pattern:
    | p = patarg
        { PATERpatar (p) }
    
    | u = uident lp = nonempty_list(patarg)
        { PATERjspquelnom (u, lp) }    

constant:
    | c = CST
        { c }
;

atom:
    | c = constant
        { Aconst (c) }

    | l = lident
        { Alident (l) }
    
    | u = uident
        { Auident (u) }
    
    | LEFTPAR e = expr RIGHTPAR
        { Aexpr (e) }
;

expr:
    | a = atom
        { Eatom (a) }
    
    | l = lident la = nonempty_list(atom)
        { Efonct (l, la) }
    
    | u = uident la = nonempty_list(atom)
        { Emodule (u, la) }
    
    | lhs = expr op = binop rhs = expr
        { Ebinop (op, lhs, rhs) } 

    | IF cond = expr THEN then_ = expr ELSE else_ = expr
        { Econd (cond, then_, else_) }
    
    | DO LEFTBRACE le = nonempty_list(expr) SEMICOLON RIGHTBRACE
        { Edo (le) }
    
    | LET LEFTBRACE lbi = nonempty_list(binding) SEMICOLON RIGHTBRACE IN e = expr
        { Eaffect (lbi, e) }
    
     | CASE cond_ = expr OF LEFTBRACE lbranch = nonempty_list(branch) SEMICOLON RIGHTBRACE
        { Ecase (cond_, lbranch) }
;

binding:
    | l = lident EQ e = expr
        { Baffect (l, e) }

branch:
    | p = pattern ARROW e = expr
        { Barrow (p, e) }

lident:
    | s = LIDENT
        { s }
;

uident:
    | s = UIDENT
        { s }
;

ident:
    | s = IDENT
        { s }
    

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
 