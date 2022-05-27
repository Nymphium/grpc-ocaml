open Core
open Fn

open struct
  module PB = Ocaml_protoc_plugin
  module Parse = Parse
  module Hex = Grpc_basic.Utils.Hex

  (* a gRPC request is compressed or not (assume uncompressed) *)
  let compressed = false

  (* add 5byte gRPC offset (compression-flag + message-length) to request body *)
  let[@inline] offset_request_body decode t =
    (* compression-flag -> {1 byte} as unsigned int *)
    let cflag = Bool.to_int compressed |> Hex.encode in
    let b = t |> decode |> PB.Writer.contents in
    (* message-length -> {4 bytes} as unsigned int (big endian) *)
    let length = String.length b |> Hex.encode ~min_digit:4 in
    (* Request -> compression-flag + *(message-length + message) *)
    Hex.concat [ cflag; length; Bytes.of_string b ]
  ;;

  let%test_module _ =
    (module struct
      (* empty protobuf decoder *)
      let to_proto =
        let apply ~f:f' () = f' [] in
        let spec = PB.Runtime.Runtime'.Serialize.C.nil in
        let serialize = PB.Runtime.Runtime'.Serialize.serialize [] spec in
        fun t -> apply ~f:serialize t
      ;;

      let offs = offset_request_body to_proto ()
      let cflag = Bytes.sub offs ~pos:0 ~len:1
      let entire_length = Bytes.length offs
      let length = Bytes.sub offs ~pos:1 ~len:4

      (* decoded length of body *)
      let length' =
        length
        |> Bytes.fold ~init:(0, 0) ~f:(fun (idx, acc) c ->
               idx + 1, acc + (Char.to_int c * Float.to_int (0xff. ** Float.of_int idx)))
        |> snd
      ;;

      let%test _ =
        (* compress segment reflects `compressed` flag *)
        Bytes.equal cflag
        @@ Bytes.of_string
        @@
        match compressed with
        | true -> "\001"
        | false -> "\000"
      ;;

      let%test _ =
        (* `length` segment is made of "valid" characters  *)
        length |> Bytes.fold ~init:true ~f:(fun b c -> b && Char.to_int c < 0x100)
      ;;

      let%test _ =
        (* length of (compress flag ^ length segment) is 5  *)
        entire_length - 5 = length'
      ;;
    end)
  ;;

  (* HTTP POST method wrapper: consider whether request is failed or not with "grpc-status" and "grpc-message" from trailer *)
  let post conn ?request_id timeout body uri =
    let headers =
      Option.fold
        request_id
        ~f:(Fn.flip @@ Grpc_basic.Headers.add "x-request-id")
        ~init:(Grpc_basic.Headers.Timeout.set_second timeout Grpc_basic.Headers.default)
    in
    let io =
      Lwt.catch
        (fun () ->
          Lwt_unix.with_timeout (Float.of_int timeout)
          @@ fun () -> Lwt.map Result.return @@ Post.call conn uri ~headers body)
        (function
          | Lwt_unix.Timeout ->
            let () =
              Lwt.async
              @@ fun () ->
              Logs_lwt.err ~src:Log.src
              @@ fun m ->
              m ~header:Log.header ~tags:Log.(Tag.empty |> Tag.uri uri) "request timeout"
            in
            Lwt_result.fail @@ (`DEADLINE_EXCEEDED, Some "request timeout")
          | exn -> Lwt_result.fail @@ (`INTERNAL, Some (Exn.to_string exn)))
    in
    flip Lwt.map io
    @@ flip Result.( >>= )
    @@ fun Post.{ status; headers; body; error = _; trailers } ->
    if H2.Status.is_error status
    then
      Error
        ( `INTERNAL
        , Some (Printf.sprintf "server side error: %s" @@ H2.Status.to_string status) )
    else (
      let op =
        let open Option in
        Grpc_basic.Headers.Trailers.get_grpc_status headers
        >>= int_of_string_opt
        >>= Grpc_basic.Error.of_enum
      in
      match op with
      | None -> Ok (Buffer.contents body, trailers)
      | Some error_status ->
        Error (error_status, Grpc_basic.Headers.Trailers.get_grpc_status headers))
  ;;
end

type t =
  { pool : Pool.t
  ; unary :
      'req 'res.
      (module Ocaml_protoc_plugin.Service.Rpc
         with type Request.t = 'req
          and type Response.t = 'res)
      -> ?timeout:int
      -> ?request_id:string
      -> 'req
      -> ('res * Grpc_basic.Headers.t, Grpc_basic.Error.t * string option) Lwt_result.t
  }

(**
 [make service_addr service'] makes gRPC client.
 @param ?pool_size: optional int as connection pool size (must be >= 1)
 @param ?timeout: optional int as timeout (second)
 @param ?request_id: optional string as "x-request-id" for tracking forwarding request
 *)
let make
    ?(* TODO: make map with address and client *)
     ping_duration
    ?ping_times
    ?(pool_size = 10)
    ?(scheme = "http")
    ~host
    ~port
    ()
  =
  assert (pool_size >= 1);
  let pool = Pool.make ?ping_times ?ping_duration ~host ~port ~pool_size () in
  let unary
      : type req res.
        (module Ocaml_protoc_plugin.Service.Rpc
           with type Request.t = req
            and type Response.t = res)
        -> ?timeout:int
        -> ?request_id:string
        -> req
        -> (res * Grpc_basic.Headers.t, Grpc_basic.Error.t * string option) Lwt_result.t
    =
   fun service' ?(timeout = 5) ?request_id req ->
    let path =
      let module S = (val service') in
      S.name' ()
    in
    let uri = Uri.make ~scheme ~host ~port ~path () in
    let decoder, encoder = PB.Service.make_client_functions' service' in
    let parse = Parse.make ~encoder in
    let io =
      let body = Bytes.to_string @@ offset_request_body decoder req in
      Lwt_pool.use pool @@ fun conn -> post ?request_id conn timeout body uri
    in
    flip Lwt.map io
    @@ flip Result.( >>= )
    @@ fun (body, trailers) ->
    let open Result.Let_syntax in
    let%bind res =
      parse body
      |> Result.map_error ~f:(function
             | `Protoc err ->
               `FAILED_PRECONDITION, Some (Ocaml_protoc_plugin.Result.show_error err)
             | #Parse.Error.t as err -> `FAILED_PRECONDITION, Some (Parse.Error.show err))
    in
    let%bind grpc_status =
      let%bind v =
        Grpc_basic.Headers.Trailers.get_grpc_status trailers
        |> Result.of_option ~error:(`INTERNAL, Some "missing grpc-status field")
      in
      int_of_string_opt v
      |> Result.of_option
           ~error:(`INTERNAL, Some (Printf.sprintf "invalid value of grpc-status: %s" v))
    in
    if grpc_status <> 0
    then
      Error
        (match Grpc_basic.Error.of_enum grpc_status with
        | Some err -> err, Grpc_basic.Headers.Trailers.get_grpc_message trailers
        | None -> `INTERNAL, Some (Printf.sprintf "unknown status %d" grpc_status))
    else Ok (res, trailers)
  in
  { pool; unary }
;;
