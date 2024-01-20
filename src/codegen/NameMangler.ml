
(** This function mangles the given function [name] depending on its parameter
  * types and attached instances. This is intended to have an unique identifier
  * for each monomorphization of a function and for each instances.
  *
  * The name mangling scheme is explained below:
  *   - The mangled name start with "_P"
  *   - Then the full name of the function is emitted:
  *      - First the instance name is emitted (if any).
  *      - Then the function name.
  *      - Both identifiers are Run-Length Encoded (RLE). Their length
  *        is emitted and then the text itself.
  *   - Finally, the parameters types of the function are encoded:
  *      - The `Effect Unit` is encoded as `u`
  *      - The `Int` is encoded as `i`
  *      - The `Boolean` is encoded as `b`
  *      - The `String` is encoded as `s`
  *      - Any other data type is encoded as follow:
  *         - First, the letter "D" is emitted.
  *         - Then its name, run-length encoded.
  *         - At last, the list of arg types is emitted between a pair "A" and "E".
  *           The types are mangled recursively.
  *
  * Some examples:
  *  - `main :: Effect Unit` is mangled as `_P4mainu`
  *  - `ack :: Int -> Int -> Int` is mangled as `_P3ackiii`
  *  - The `show` function of `Show Int` is mangled as `_P4Showi4showiii`
  *    (in reality this show function is builtin and implemented as a native C
  *     function, therefore it is not mangled like this for now).
  *)
let mangle name param_types =
  let buffer = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buffer in

  Format.fprintf ppf "_P%d%s" (String.length name) name;

  let rec pp_type ppf typ =
    match TyperCommon.head typ with
    | TypedTree.Ttyp_data (name, args) as typ ->
        if typ = TypedTree.effect_unit then Format.fprintf ppf "u"
        else if typ = TypedTree.boolean_type then Format.fprintf ppf "b"
        else if typ = TypedTree.int_type then Format.fprintf ppf "i"
        else if typ = TypedTree.string_type then Format.fprintf ppf "s"
        else
          (* Generic mangling for other data types. *)
          Format.fprintf ppf "D%d%sA%aE" (String.length name) name
            (Format.pp_print_list ~pp_sep:(fun _ () -> ()) pp_type)
            args
    | TypedTree.Ttyp_variable _ ->
        failwith
          "When compiled, there should be no more unresolved variables of type."
  in

  List.iter (pp_type ppf) param_types;

  Format.pp_print_flush ppf ();
  Buffer.contents buffer
