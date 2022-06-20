module Const = Grpc_basic.Const
module Headers = Grpc_basic.Headers
module Hex = Grpc_basic.Utils.Hex

(** convert empty response to `0 length as 4 byte big endian`
    and add gRPC frames: Compressed-Flag Message-Length Message *)
let trim_response_with_frames msg =
  let len, msg =
    let len = String.length msg in
    (* Hex.(encode ~min_digit:Const.message_length_length 0) *)
    if len = 0 then 4, "\000\000\000\000" else len, msg
  in
  let len' = Hex.encode len ~min_digit:Grpc_basic.Const.message_length_length in
  let cflag = Const.uncompressed_flag in
  String.concat "" [ cflag; Hex.unsafe_to_string len'; msg ]
;;

let ok_trailers =
  Headers.(
    (* (Grpc_basic.status_to_int `OK |> string_of_int) *)
    empty |> Trailers.set_grpc_status "0")
;;

let make_response_trailers = function
  | Ok msg -> trim_response_with_frames msg, ok_trailers
  | Error (status, msg) ->
    let msg = Option.value msg ~default:"" in
    let trailers =
      Headers.(
        empty
        |> Trailers.set_grpc_status (Grpc_basic.status_to_int status |> string_of_int)
        |> Trailers.set_grpc_message msg)
    in
    trim_response_with_frames "", trailers
;;

(** @see https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md#requests *)
let decode_body_frame body =
  let entire_len = String.length body in
  let len =
    Const.(String.sub body compress_flag_length message_length_length)
    |> Hex.unsafe_of_string
    |> Hex.decode
  in
  let body = String.sub body Const.frame_offset (entire_len - Const.frame_offset) in
  let actual_len = String.length body in
  if len = actual_len
  then (
    let compressed = Char.equal body.[0] Const.compressed_flag_c in
    Lwt.return (compressed, body))
  else (
    let () =
      Lwt.async
      @@ fun () ->
      Logs_lwt.err ~src:Log.src
      @@ fun m ->
      m
        "invalid message length (expected %d, actual %d)"
        actual_len
        len
        ~header:Log.header
    in
    Lwt.fail_with "invalid message length")
;;
