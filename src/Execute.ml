open LibIris

(** The C compiler used to assemble and link the output assembly. *)
let cc = "gcc"

let compile_asm out_file_name typed_f =
  let ctx = Ir.mk_ctx () in

  (* Generate the Iris intermediate representation.
     The IR is stored inside ctx. *)
  CodeGen.codegen ctx typed_f;

  (* Run optimizations and assembler generation. *)
  let pm = PassManager.create Backend.Arch_x64 in
  let out = open_out out_file_name in
  PassManager.run_on_ctx pm out ctx;
  close_out out

let compile_asm_to_tmp typed_f =
  let tmp_name = Filename.temp_file "ppurs_" ".s" in
  compile_asm tmp_name typed_f;
  tmp_name

let compile out_file_name typed_f =
  let asm_tmp_name = compile_asm_to_tmp typed_f in
  let gcc_pid =
    Unix.create_process cc
      [| cc; asm_tmp_name; "-o"; out_file_name; "prt/libprt.a" |]
      Unix.stdin Unix.stdout Unix.stderr
  in
  let _, process_status = Unix.waitpid [] gcc_pid in
  Sys.remove asm_tmp_name;
  match process_status with
  | Unix.WEXITED exit_code when exit_code = 0 -> true
  | Unix.WEXITED exit_code ->
      Format.eprintf "%s returned %d exit status@." cc exit_code;
      false
  | _ ->
      Format.eprintf "%s crashed@." cc;
      false

let compile_to_tmp typed_f =
  let out_tmp_name = Filename.temp_file "ppurs_" ".exe" in
  if compile out_tmp_name typed_f then Some out_tmp_name else None

let execute typed_f =
  let exe_tmp_name = compile_to_tmp typed_f in
  match exe_tmp_name with
  | None -> None
  | Some exe_tmp_name -> (
      (* Make it executable. *)
      Unix.chmod exe_tmp_name 00100;
      let program_pid =
        Unix.create_process exe_tmp_name [| exe_tmp_name |] Unix.stdin
          Unix.stdout Unix.stderr
      in
      let _, process_status = Unix.waitpid [] program_pid in
      Sys.remove exe_tmp_name;
      match process_status with
      | Unix.WEXITED exit_code -> Some exit_code
      | _ -> None)
