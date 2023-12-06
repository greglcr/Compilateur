type t = {
    (* The source file name. *)
    file : string;
    (* The line number. The first line is numbered 1. *)
    lineno : int;
    (* The column number. The first column is numbered 1. *)
    colno : int;
}
