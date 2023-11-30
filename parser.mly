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
%token ARROW FATARROW

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
;


defn:
    | lid = lident p = list (partag) EQUAL e = expr
        { DEF (lid, p, e) } 
;


partag:
    | c = constant
        { PATARconst (c) }
    
    | l = lident
        { PATARlid (l) }
    
    | u = uident
        { PATARuid (u) }
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
    
    (* | CASE e = expr OF LEFTBRACE lbranch = nonempty_list(branch) SEMICOLON RIGHTBRACE
        { Ecase (e, lbranch) } *)
;

binding:
    | l = lident EQUAL e = expr
        { Baffect (l, e) }

branch:
    | p = pattern ARROW expr
        { }

constant:
    | c = CST
        { c }
;


lident:
    | s = LIDENT
        { s }
;


uident:
    | s = UIDENT
        { s }
;
    


 