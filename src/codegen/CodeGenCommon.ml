open TypedTree
open LibIris

(** Converts the given MiniPureScript type to a compatible Iris type. *)
let convert_type typ =
  let typ = TyperCommon.head typ in

  (* We use 64-bit integer to represent boolean and integers. Everything else
     is allocated on the heap and therefore is represented with
     a pointer. *)
  if typ = boolean_type || typ = int_type then Ir.Ityp_int else Ir.Ityp_ptr

let mk_alloc ib typ =
  let builtin_alloc =
    Ir.get_or_insert_fn ib.IrBuilder.ctx "__prt_alloc" ~is_external:true
      Ityp_ptr [ Ityp_int ]
  in
  (* FIXME: too specific to x86 *)
  let size_in_bytes = X86MrBuilder.sizeof_operand typ in
  IrBuilder.mk_call ib builtin_alloc [ Ir.Iop_imm (Z.of_int size_in_bytes) ]
