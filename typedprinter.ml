open Typedtree
open Format

let rec pp_list pp_v fmt = function
  | [] -> ()
  | [x] -> pp_v fmt x
  | x :: xs ->
      fprintf fmt "%a,@ %a" pp_v x (pp_list pp_v) xs

let rec pp_typed_node pp_a fmt node =
  fprintf fmt "{@[type = %a;@;node = %a@]}" pp_typ node.typ pp_a node.node

and pp_typ fmt typ = match Typer.head typ with
  | Ttyp_unit -> fprintf fmt "unit"
  | Ttyp_boolean -> fprintf fmt "Boolean"
  | Ttyp_int -> fprintf fmt "Int"
  | Ttyp_string -> fprintf fmt "String"
  | Ttyp_effect t -> fprintf fmt "Effect %a " pp_typ t
  | Ttyp_variable tv -> fprintf fmt "t%d" tv.id
  | Ttyp_function (args, ret) ->
      fprintf fmt "(%a) -> %a"
        (pp_list pp_typ) args
        pp_typ ret
  | Ttyp_data (name, args) ->
      fprintf fmt "data %s %a" name (pp_list pp_typ) args

and pp_decl_kind fmt = function
  | Tdecl_function (name, params, expr) ->
      fprintf fmt "FUNC (@[name = %s,@ params = %a,@ body = %a@])"
        name.node
        (pp_list pp_param) params
        pp_expr expr
  | Tdecl_data (name, constructors) ->
      fprintf fmt "DATA (@[name = %s,@ constructors = %a@])" name.node (pp_list pp_constructor) constructors

and pp_constructor fmt (name, args) =
  fprintf fmt "(@[%s,@ %a@])" name.node (pp_list pp_typ) args

and pp_param fmt (name, typ) =
  fprintf fmt "%s: %a" name.node pp_typ typ

and pp_constant fmt = function
| Ast.Cbool b -> fprintf fmt "%B" b
| Ast.Cint i -> fprintf fmt "%d" i
| Ast.Cstring s -> fprintf fmt "\"%s\"" s
| Ast.Cunit -> fprintf fmt "unit"

and pp_binop fmt = function
| Ast.Badd -> fprintf fmt "+"
| Ast.Bsub -> fprintf fmt "-"
| Ast.Bmul -> fprintf fmt "*"
| Ast.Bdiv -> fprintf fmt "/"
| Ast.Beq -> fprintf fmt "=="
| Ast.Bneq -> fprintf fmt "/="
| Ast.Blt -> fprintf fmt "<"
| Ast.Ble -> fprintf fmt "<="
| Ast.Bgt -> fprintf fmt ">"
| Ast.Bge -> fprintf fmt ">="
| Ast.Band -> fprintf fmt "&&"
| Ast.Bor -> fprintf fmt "||"
| Ast.Bconcat -> fprintf fmt "<>"

and pp_expr fmt expr =
  fprintf fmt "{@[type = %a;@;node = %a@]}" pp_typ expr.typ pp_expr_kind expr.node

and pp_decl fmt decl =
  fprintf fmt "{@[type = %a;@;node = %a@]}" pp_typ decl.typ pp_decl_kind decl.node

and pp_expr_kind fmt = function
  | Texpr_constant c -> fprintf fmt "ECST %a" pp_constant c
  | Texpr_binary (op, e1, e2) ->
      fprintf fmt "EBIN (@[binop = '%a',@ lhs = %a,@ rhs = %a@])"
        pp_binop op.node
        pp_expr e1
        pp_expr e2
  | Texpr_variable id -> fprintf fmt "VAR %s" id.node
  | Texpr_apply (id, args) ->
      fprintf fmt "EAPPLY (@[name = %s,@ args = %a@])" id.node (pp_list pp_expr) args
  | Texpr_constructor (id, args) ->
      fprintf fmt "ECONS (@[name = %s,@ args = %a@])" id.node (pp_list pp_expr) args
  | Texpr_if (cond, then_expr, else_expr) ->
      fprintf fmt "EIF (@[cond = %a,@ then = %a,@ else = %a@])"
        pp_expr cond
        pp_expr then_expr
        pp_expr else_expr
  | Texpr_do exprs ->
      fprintf fmt "EDO (@[%a@])" (pp_list pp_expr) exprs
  | Texpr_let (binding, expr) ->
      fprintf fmt "ELET (@[binding = %a,@ expr = %a@])"
        pp_binding binding
        pp_expr expr
  | Texpr_case (expr, branches) ->
      fprintf fmt "ECASE (@[cond = %a, branches = %a@])"
        pp_expr expr
        (pp_list pp_branch) branches

and pp_binding fmt (id, expr) =
  fprintf fmt "%s = %a" id.node pp_expr expr

and pp_pattern_kind fmt = function
  | Tpattern_constant c -> fprintf fmt "PCST %a" pp_constant c
  | Tpattern_variable id -> fprintf fmt "PVAR %s" id.node
  | Tpattern_constructor (id, patterns) ->
      fprintf fmt "PCONST (@[name = %s,@ args = %a@])" id.node (pp_list pp_pattern) patterns

and pp_branch fmt (pattern, expr) =
  fprintf fmt "%a -> %a"
    pp_pattern pattern
    pp_expr expr

and pp_pattern fmt pattern =
  fprintf fmt "{@[type = %a;@;node = %a@]}" pp_typ pattern.typ pp_pattern_kind pattern.node

and pp_file fmt decls =
  List.iter (fun decl -> (
    pp_decl fmt decl;
    Format.pp_print_newline fmt ();
  )) decls

and pp_hashtbl pp_v fmt tbl =
  let pp_binding fmt (key, value) =
    fprintf fmt "%a -> %a" pp_v key pp_typ value
  in
  fprintf fmt "{%a}" (pp_list pp_binding) (Hashtbl.to_seq tbl |> List.of_seq)
