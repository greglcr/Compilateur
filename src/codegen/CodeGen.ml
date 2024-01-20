open TypedTree
open Ast
open LibIris

(** Should we use native division (the idiv x86 instruction) or use the real PureScript
    division implemented in the runtime library? The later is slower but compliant
    with the PureScript implementation. *)
let native_div = ref false

let mk_div ib lhs rhs =
  if !native_div then IrBuilder.mk_sdiv ib lhs rhs
  else
    let builtin_div =
      Ir.get_or_insert_fn ib.IrBuilder.ctx "__prt_div" ~is_external:true
        Ityp_int [ Ityp_int; Ityp_int ]
    in
    IrBuilder.mk_call ib builtin_div [ Ir.Iop_reg lhs; Ir.Iop_reg rhs ]

let mk_rem ib lhs rhs =
  if !native_div then IrBuilder.mk_srem ib lhs rhs
  else
    let builtin_rem =
      Ir.get_or_insert_fn ib.IrBuilder.ctx "__prt_rem" ~is_external:true
        Ityp_int [ Ityp_int; Ityp_int ]
    in
    IrBuilder.mk_call ib builtin_rem [ Ir.Iop_reg lhs; Ir.Iop_reg rhs ]

let mk_string_concat ib lhs rhs =
  let builtin_strconcat =
    Ir.get_or_insert_fn ib.IrBuilder.ctx "__prt_strconcat" ~is_external:true
      Ityp_ptr [ Ityp_ptr; Ityp_ptr ]
  in
  IrBuilder.mk_call ib builtin_strconcat [ Ir.Iop_reg lhs; Ir.Iop_reg rhs ]

let mk_log ib s =
  let builtin_log =
    Ir.get_or_insert_fn ib.IrBuilder.ctx "__prt_log" ~is_external:true Ityp_void
      [ Ityp_ptr ]
  in
  IrBuilder.mk_call ib builtin_log [ Ir.Iop_reg s ]

let mk_show_bool ib s =
  let builtin_show_bool =
    Ir.get_or_insert_fn ib.IrBuilder.ctx "__prt_show_bool" ~is_external:true
      Ityp_ptr [ Ityp_int ]
  in
  IrBuilder.mk_call ib builtin_show_bool [ Ir.Iop_reg s ]

let mk_show_int ib s =
  let builtin_show_int =
    Ir.get_or_insert_fn ib.IrBuilder.ctx "__prt_show_int" ~is_external:true
      Ityp_ptr [ Ityp_int ]
  in
  IrBuilder.mk_call ib builtin_show_int [ Ir.Iop_reg s ]

let to_iris_type typ =
  let typ = TyperCommon.head typ in
  if typ = boolean_type || typ = int_type then Ir.Ityp_int
  else if typ = string_type then Ir.Ityp_ptr
  else Ir.Ityp_void

let rec codegen_expr ib env expr =
  match expr.TypedTree.node with
  | Texpr_constant (Cbool true) -> IrBuilder.mk_true ib
  | Texpr_constant (Cbool false) -> IrBuilder.mk_false ib
  | Texpr_constant (Cint n) -> IrBuilder.mk_int ib (Z.of_int n)
  | Texpr_constant (Cstring s) -> IrBuilder.mk_zstring ib s
  | Texpr_constant Cunit ->
      IrBuilder.mk_int ib
        Z.zero (* create a dummy instruction that will be removed by DCE *)
  | Texpr_binary ({ node = Band }, lhs, rhs) ->
      let lhs = codegen_expr ib env lhs in
      IrBuilder.mk_logical_and ib lhs (fun ib -> codegen_expr ib env rhs)
  | Texpr_binary ({ node = Bor }, lhs, rhs) ->
      let lhs = codegen_expr ib env lhs in
      IrBuilder.mk_logical_or ib lhs (fun ib -> codegen_expr ib env rhs)
  | Texpr_binary
      ({ node = (Beq | Bneq | Blt | Ble | Bgt | Bge) as op }, lhs, rhs) -> (
      let is_string = TyperCommon.head lhs.typ = string_type in
      let lhs = codegen_expr ib env lhs in
      let rhs = codegen_expr ib env rhs in

      let lhs, rhs =
        if is_string then
          let builtin_strcmp =
            Ir.get_or_insert_fn ib.IrBuilder.ctx "__prt_strcmp"
              ~is_external:true Ityp_int [ Ityp_ptr; Ityp_ptr ]
          in
          let cmp =
            IrBuilder.mk_call ib builtin_strcmp [ Iop_reg lhs; Iop_reg rhs ]
          in
          (cmp, IrBuilder.mk_int ib Z.zero)
        else (lhs, rhs)
      in

      match op with
      | Beq -> IrBuilder.mk_eq ib lhs rhs
      | Bneq -> IrBuilder.mk_ne ib lhs rhs
      | Blt -> IrBuilder.mk_slt ib lhs rhs
      | Ble -> IrBuilder.mk_sle ib lhs rhs
      | Bgt -> IrBuilder.mk_sgt ib lhs rhs
      | Bge -> IrBuilder.mk_sge ib lhs rhs
      | _ -> assert false)
  | Texpr_binary (binop, lhs, rhs) -> (
      let lhs = codegen_expr ib env lhs in
      let rhs = codegen_expr ib env rhs in
      match binop.node with
      (* Arithmetic operators *)
      | Badd -> IrBuilder.mk_add ib lhs rhs
      | Bsub -> IrBuilder.mk_sub ib lhs rhs
      | Bmul -> IrBuilder.mk_mul ib lhs rhs
      | Bdiv -> mk_div ib lhs rhs
      (* Comparison *)
      | Beq | Bneq | Blt | Ble | Bgt | Bge ->
          failwith "comparisons are already implemented separately"
      (* Logical operators *)
      | Band | Bor -> failwith "Band and Bor are already implemented separately"
      (* String concat *)
      | Bconcat -> mk_string_concat ib lhs rhs)
  | Texpr_variable var -> SMap.find var.spelling env
  | Texpr_apply (name, args) -> (
      (* We have special cases for some builtin functions for performance reasons. *)
      match name.spelling with
      | "not" -> (
          match args with
          | [ arg ] ->
              let arg = codegen_expr ib env arg in
              (* not is a bitwise not which is not the same as the expected
                 logical not of PureScript. We must only keep the least
                 significant bit (AND with 1). *)
              let not = IrBuilder.mk_not ib arg in
              let one = IrBuilder.mk_int ib Z.one in
              IrBuilder.mk_and ib not one
          | _ -> failwith "invalid arguments for not builtin function")
      | "mod" -> (
          match args with
          | [ lhs; rhs ] ->
              let lhs = codegen_expr ib env lhs in
              let rhs = codegen_expr ib env rhs in
              mk_rem ib lhs rhs
          | _ -> failwith "invalid arguments for mod builtin function")
      | "log" -> (
          match args with
          | [ s ] ->
              let s = codegen_expr ib env s in
              mk_log ib s
          | _ -> failwith "invalid arguments for log builtin function")
      | "show" -> (
          match args with
          | [ s ] ->
              let scg = codegen_expr ib env s in
              let typ = TyperCommon.head s.typ in
              if typ = int_type then mk_show_int ib scg
              else if typ = boolean_type then mk_show_bool ib scg
              else failwith "unexpected type for show builtin function"
          | _ -> failwith "invalid arguments for show builtin function")
      | _ ->
          let args =
            List.map (fun arg -> Ir.Iop_reg (codegen_expr ib env arg)) args
          in
          let callee = Option.get (Ir.get_fn ib.ctx name.spelling) in
          IrBuilder.mk_call ib callee args)
  | Texpr_constructor _ -> failwith "TODO: codegen constructors"
  | Texpr_if (cond, then_expr, else_expr) ->
      let cond = codegen_expr ib env cond in
      IrBuilder.mk_if_expr ib cond
        (fun ib -> codegen_expr ib env then_expr)
        (fun ib -> codegen_expr ib env else_expr)
  | Texpr_do exprs ->
      (* The function requires a return value, but all expressions have a void type.
         The solution: adding a dummy constant instruction that will be removed during
         optimizations (it is unused, DCE pass will remove it). *)
      let dummy = IrBuilder.mk_int ib Z.zero in
      List.iter (fun expr -> ignore (codegen_expr ib env expr)) exprs;
      dummy
  | Texpr_let ((name, init_expr), expr) ->
      let var_value = codegen_expr ib env init_expr in
      codegen_expr ib (SMap.add name.spelling var_value env) expr
  | Texpr_case _ -> failwith "TODO: codegen cases"

let mangle_func_name name =
  if name.spelling = "main" then "__ppurs_main" else name.spelling

let codegen_fn ctx name params body =
  let fn_name = mangle_func_name name in
  let fn = Option.get (Ir.get_fn ctx fn_name) in
  let ib = IrBuilder.create_for_fn fn in

  (* FIXME: Move this to Iris IrBuilder API. *)
  ib.cur_func_decl <- Some fn;
  let entry_bb = IrBuilder.mk_bb ib in
  fn.fn_entry <- Some entry_bb.b_label;
  IrBuilder.set_bb ib entry_bb;

  let ret_value = codegen_expr ib SMap.empty body in

  if body.typ = effect_unit then IrBuilder.set_term ib Iterm_ret
  else IrBuilder.set_term ib (Iterm_retv (Iop_reg ret_value));
  IrBuilder.finish ib

let codegen ctx decls =
  (* Register functions in Iris. *)
  List.iter
    (fun decl ->
      match decl.TypedTree.node with
      | Tdecl_function (name, params, body) ->
          let fn_name = mangle_func_name name in
          let iris_params =
            List.map (fun (_, typ) -> to_iris_type typ) params
          in
          let iris_retty = to_iris_type body.typ in
          let fn = Ir.get_or_insert_fn ctx fn_name iris_retty iris_params in
          ignore fn
      | _ -> failwith "TODO")
    decls;

  (* Compile functions. *)
  List.iter
    (fun decl ->
      match decl.TypedTree.node with
      | Tdecl_function (name, params, body) -> codegen_fn ctx name params body
      | _ -> failwith "TODO")
    decls
