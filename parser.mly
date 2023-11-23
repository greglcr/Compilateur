%{
    open Ast
%}

%token <Ast.constant> CST (*Lexème qui va ratourner une constante. Ici on peut directement renvoyer le type
                            correspondant aux constantes de la syntaxe abstraite car il n'y a qu'une seule
                            possibilité*)

%token <Ast.binop> BINOP (*Pareil*)
%token CASE CLASS DATA DO ELSE FALSE FORALL IF IMPORT IN INSTANCE LET MODULE OF THEN TRUE WHERE
%token LEFTPAR RIGHTPAR
%token LEFTBRACE RIGHTBRACE
%token EOF
%token <string> IDENT
%start file

%type <Ast.expr> file

%%

file:
    | EOF
        { Econstant (Cbool (false) ) }


 