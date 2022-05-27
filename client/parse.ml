open struct
  module PB = Ocaml_protoc_plugin
  include Core
  include Grpc_basic.Const

  let split_at str idx =
    let lhs =
      String.fold_until
        ~init:(0, [])
        ~f:(fun (idx', acc) c ->
          if idx' >= idx
          then Continue_or_stop.Stop acc
          else Continue_or_stop.Continue (idx' + 1, c :: acc))
        ~finish:snd
        str
      |> List.rev
      |> String.of_char_list
    in
    lhs, String.drop_prefix str idx
  ;;

  let%test_unit _ =
    Quickcheck.test
      (Quickcheck.Generator.tuple2
         String.quickcheck_generator
         Quickcheck.Generator.small_non_negative_int)
      ~f:(fun (str, idx) ->
        let str_len = String.length str in
        let lhs, rhs = split_at str idx in
        [%test_eq: string] (lhs ^ rhs) str;
        if String.is_empty str
        then (
          [%test_eq: string] rhs "";
          [%test_eq: string] rhs "");
        if idx >= str_len then [%test_eq: string] lhs str)
  ;;

  let assrt ~fail b = if not b then Result.fail fail else Result.return ()

  let decode_4byte_int str =
    let len = String.length str in
    if len <> message_length_length
    then
      invalid_argf
        "%s: expected string with length greater than or equal to 4 (got %d)"
        __FUNCTION__
        len
        ()
    else
      str
      |> String.to_list_rev
      |> List.foldi ~init:0 ~f:(fun idx acc c ->
             (Int.pow 0x100 idx * Char.to_int c) + acc)
  ;;

  (** splits ls with ~f and returns (lhs, e, rhs) and e is the leftmost one which satisfies f *)
  let drop_elt ~f ls =
    let f = Fn.compose not f in
    let lhs = List.take_while ~f ls in
    let rhs' = List.drop_while ~f ls in
    match rhs' with
    | [] -> lhs, None, []
    | e :: tl -> lhs, Some e, tl
  ;;

  let%test_unit _ =
    Quickcheck.test
      (Quickcheck.Generator.tuple2
         (List.quickcheck_generator Int.quickcheck_generator)
         (Quickcheck.Generator.fn Int.quickcheck_observer Bool.quickcheck_generator))
      ~f:(fun (list, f) ->
        let lhs, v, rhs = drop_elt ~f list in
        match v with
        | None -> [%test_eq: int list] (lhs @ rhs) list
        | Some v -> [%test_eq: int list] (lhs @ (v :: rhs)) list)
  ;;
end

module Error = struct
  type t =
    [ `Compressed of string
    | `Invalid_content
      (** `Invalid_content appears when the given content has length n and message length m, and n != m *)
    ]
  [@@deriving show { with_path = false }]
end

let make
    :  encoder:(PB.Reader.t -> ('ok, 'err) Result.t) -> string
    -> ('ok, [> Error.t ]) Result.t
  =
 fun ~encoder ->
  let encoder s =
    s |> PB.Reader.create |> encoder |> Result.map_error ~f:(fun err -> `Protoc err)
  in
  (* parse top level considering offset and empty body *)
  fun msg ->
    let open Result.Let_syntax in
    let cflag, rest = split_at msg compress_flag_length in
    let%bind () =
      assrt (String.equal cflag uncompressed_flag) ~fail:(`Compressed "message")
    in
    let length, message =
      split_at rest message_length_length |> Tuple2.map_fst ~f:decode_4byte_int
    in
    let%bind () = assrt (length = String.length message) ~fail:`Invalid_content in
    encoder message
;;
