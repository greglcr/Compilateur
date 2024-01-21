open TypedTree
open LibIris
open CodeGenCommon

let type_of_constructor (_, args) =
  let iris_args = List.map convert_type args in
  (* We add an initial integer to store the discriminant of the constructor
     (the unique id of the constructor among its parent data type). *)
  Ir.Ityp_struct (Ir.Ityp_int :: iris_args)

let codegen_constructor_call ib const args =
  let typ = type_of_constructor const in
  let addr = mk_alloc ib typ in
  List.iteri (fun arg i -> IrBuilder.mk_storefield ib typ addr i arg) args

let constructor_discriminant ib addr =
  (* The discriminant is stored at the first field. Therefore, reading it is
     the same as loading the first integer at [addr]. Another way will be to
     create a struct type of one integer field and loading the first field of
     this struct. *)
  IrBuilder.mk_load ib Ityp_int addr
