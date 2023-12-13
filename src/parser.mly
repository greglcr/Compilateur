%{

    open Ast
    open Format

    module Imports = Set.Make(String)

    let mk_node (s, e) node =
        {
            range = (Location.from_lexing_position s, Location.from_lexing_position e);
            node
        }

    let mk_expr range expr_kind = { expr_kind; expr_range = Location.from_lexing_range range }
    let mk_decl range decl_kind = { decl_kind; decl_range = Location.from_lexing_range range }
    let mk_type range type_kind = { type_kind; type_range = Location.from_lexing_range range }
    let mk_pattern range pattern_kind = { pattern_kind; pattern_range = Location.from_lexing_range range }
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
%type <Ast.program> file

%%


file:
    | MODULE name = uident WHERE LBRACE
        imports = imports
        decls = separated_nonempty_list(SEMI, decl)
      RBRACE EOF
        {
            let dummy_range = Location.dummy, Location.dummy in
            if not (Imports.mem "Prelude" imports) then
                raise (Semantic_error (dummy_range, "missing 'import Prelude'"));
            if not (Imports.mem "Effect" imports) then
                raise (Semantic_error (dummy_range, "missing 'import Effect'"));
            if not (Imports.mem "Effect.Console" imports) then
                raise (Semantic_error (dummy_range, "missing 'import Effect.Console'"));

            if name.node <> "Main" then
                raise (Semantic_error (name.range, "expected 'Main' as module name"));

            decls
        }
;

(* For our limited implementation of MiniPureScript, we don't support imports.
   However, we expect programs to contain some required imports to be compilable
   by the reference implementation of PureScript. To check if these imports
   exist, we accumulate all imports into a set (we don't check duplicates because,
   in PureScript, they are allowed). Then, we check if the import set contains
   the required imports. *)
imports:
    | IMPORT name = UIDENT SEMI
        {
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

    | d = decl_kind
        { mk_decl $loc d }
;

decl_kind:
    | DATA name = uident targs = lident* EQ constructors = separated_nonempty_list(PIPE, constructor)
        { Pdecl_data (name, targs, constructors) }

    | CLASS name = uident targs = lident* WHERE LBRACE decls = separated_list(SEMI, tdecl) RBRACE
        { Pdecl_class (name, targs, decls) }

    | INSTANCE i = instance WHERE LBRACE decls = separated_list(SEMI, defn) RBRACE
        { Pdecl_instance (i, decls) }
;

constructor:
    | name = uident args = atype*
        { name, args }
;

defn:
    | name = LIDENT p = patarg* EQ e = expr
        { mk_decl $loc (Pdecl_equation (mk_node $loc(name) name, p, e)) }
;

tdecl:
    | name = lident COLON_COLON gen_vars = forall
      typs = separated_nonempty_list(ARROW, typ)
        {
            let rev_typs = List.rev typs in
            let last_typ = List.hd (List.rev typs) in
            mk_decl $loc (Pdecl_function
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
        { mk_type $loc (Ptyp_variable (name)) }

    | name = UIDENT
        { mk_type $loc (Ptyp_data (mk_node $loc(name) name, [])) }

    | LPAR t = typ RPAR
        { t }
;

ntype:
    | name = uident args = atype*
        { mk_type $loc (Ptyp_data (name, args)) }
;

typ:
    | name = LIDENT
        { mk_type $loc (Ptyp_variable (name)) }

    | name = uident args = typ*
        { mk_type $loc (Ptyp_data (name, args)) }

    | LPAR t = typ RPAR
        { t }
;

instance:
    | nt = ntype
        { [ ], nt }

    | nt1 = ntype FAT_ARROW nt2 = ntype
        { ([nt1], nt2) }

    | LPAR lnt = separated_nonempty_list(COMMA, ntype) RPAR FAT_ARROW nt = ntype
        { (lnt, nt) }
;

patarg:
    | c = CST
        { mk_pattern $loc (Ppattern_constant (c)) }

    | name = lident
        { mk_pattern $loc (Ppattern_variable (name)) }

    | name = uident
        { mk_pattern $loc (Ppattern_constructor (name, [])) }

    | LPAR p = pattern RPAR
        { p }
;

pattern:
    | p = patarg
        { p }

    | name = uident args = patarg+
        { mk_pattern $loc (Ppattern_constructor (name, args)) }

(* atom without location *)
atom:
    | c = CST
        { mk_expr $loc (Pexpr_constant (c)) }

    | name = lident
        { mk_expr $loc (Pexpr_variable name) }

    | name = uident
        { mk_expr $loc (Pexpr_constructor (name, [])) }

    | LPAR e = expr RPAR
        { e }
;

expr:
    | a = atom
        { a }

    | e = expr_kind
        { mk_expr $loc e }

(* expr without location *)
expr_kind:
    | name = lident args = atom+
        { Pexpr_apply (name, args) }

    | name = uident args = atom+
        { Pexpr_constructor (name, args) }

    | MINUS e = expr %prec UNARY_MINUS
        { Pexpr_neg (e) }

    | lhs = expr op = binop rhs = expr
        { Pexpr_binary (mk_node $loc(op) op, lhs, rhs) }

    | IF cond = expr THEN then_ = expr ELSE else_ = expr
        { Pexpr_if (cond, then_, else_) }

    | DO LBRACE body = separated_nonempty_list(SEMI, expr) RBRACE
        { Pexpr_do (body) }

    | LET LBRACE bindings = separated_nonempty_list(SEMI, binding) RBRACE IN e = expr
        { Pexpr_let (bindings, e) }

    | CASE cond = expr OF LBRACE lbranch = separated_nonempty_list(SEMI, branch) RBRACE
        { Pexpr_case (cond, lbranch) }
;

%inline binding:
    | name = lident EQ e = expr
        { (name, e) }

%inline branch:
    | p = pattern ARROW e = expr
        { (p, e) }

%inline lident:
    | ident = LIDENT
        { mk_node $loc ident }
;

%inline uident:
    | ident = UIDENT
        { mk_node $loc ident }
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
