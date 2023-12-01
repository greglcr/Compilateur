%{

    open Ast
    open Format

%}

%token FILEINIT IMPORTINIT
%token <Ast.constant> CST (* Lexème qui va retourner une constante. Ici on peut directement renvoyer le type
                            correspondant aux constantes de la syntaxe abstraite car il n'y a qu'une seule
                            possibilité *)

%token <Ast.binop> BINOP (* Pareil *)
%token MINUS (* Seul token qu'on doit traiter à part car c'est le seul qui peut être utilisé devant une
                expression *)
%token EQUAL
%token CASE CLASS DATA DO ELSE FALSE FORALL IF IMPORT IN INSTANCE LET MODULE OF THEN TRUE WHERE
%token <string> LIDENT UIDENT
%token LEFTPAR RIGHTPAR
%token LEFTBRACE RIGHTBRACE
%token SEMICOLON
%token EOF
%token <string> IDENT
%token ARROW FAT_ARROW
%token TWO_PTS INTE_POINT
%token POINT

%start file (* Seules les fonctions qui sont placées après un start seront copiées dans le fichier .mli,
                donc seules celles là pourront être appelées en dehors de ce fichier *)

%type <Ast.file> file

%%


file:
    | FILEINIT LEFTBRACE i = import d = nonempty_list(decl) SEMICOLON RIGHTBRACE EOF
        { Fprogramm d }
;


import:
    | IMPORTINIT
        {}
;


decl:
    | d = defn
        { DECLdefn (d) }
    
    | td = tdecl
        { DECLtdecl (td) }

    | DATA u1 = uident lli = list(lident) EQUAL WHERE lpair = nonempty_list(uident_latype)
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
    | lid = lident p = list (patarg) EQUAL e = expr
        { DEF (lid, p, e) }
;

tdecl:
    | li = lident TWO_PTS LEFTPAR FORALL lli = nonempty_list(lident) POINT RIGHTPAR INTE_POINT
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
    
    | e1 = expr b = BINOP e2 = expr
        { Ebinop (b, e1, e2) } 

    | IF e1 = expr THEN e2 = expr ELSE e3 = expr
        { Econd (e1, e2, e3) }
    
    | DO LEFTBRACE le = nonempty_list(expr) SEMICOLON RIGHTBRACE
        { Edo (le) }
    
    | LET LEFTBRACE lbi = nonempty_list(binding) SEMICOLON RIGHTBRACE IN e = expr
        { Eaffect (lbi, e) }
    
     | CASE e = expr OF LEFTBRACE lbranch = nonempty_list(branch) SEMICOLON RIGHTBRACE
        { Ecase (e, lbranch) }
;

binding:
    | l = lident EQUAL e = expr
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
    


 