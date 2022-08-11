open Core
open Fn

open struct
  module PB = Ocaml_protoc_plugin
  module Context = Grpc_core.Context
  module Middlewares = Grpc_core.Server.Middlewares

  let default_tags ctx =
    let target' = Context.get_target ctx in
    let host = Context.get_host ctx in
    Log.Tag.(empty |> target target' |> str ~name:"host" host)
  ;;
end

type ('req, 'res) unary =
  (module PB.Service.Rpc with type Request.t = 'req and type Response.t = 'res)
  -> ('req, 'res) Grpc_core.Server.handler
  -> Grpc_core.Server.t
  -> Grpc_core.Server.t

(** make success response  *)
let ok ?(metadata = []) res : 'a Grpc_core.Protoiso.res = Result.return (res, metadata)

(** composition of [ok] and [Lwt.return] *)
let return ?metadata res = Lwt.return @@ ok ?metadata res

(** make error response *)
let fail ?(status = `INTERNAL) ?(metadata = []) ?detail () : 'a Grpc_core.Protoiso.res =
  Result.fail (status, detail, metadata)
;;

(** composition of [fail] and [Lwt.return] *)
let fail' ?status ?metadata ?detail () = Lwt.return @@ fail ?status ?metadata ?detail ()

(** Unary rpc handler *)
module Unary = struct
  (** adds unary handler:
      adds `host` and `target` to its context, and logs `target` and duration of execution time when handling
{
Unary.add rpc @@ fun ctx req md ->
  let%lwt () = Lwt_io.printl (Context.get_host ctx) in (* prints the host of client *)
  ......
} *)
  let add : type req res. (req, res) unary =
   fun rpc ->
    let module R = (val rpc) in
    let decode, encode = PB.Service.make_service_functions' rpc in
    let decode body =
      PB.Reader.create body
      |> decode
      |> Result.map_error ~f:(fun err ->
             `FAILED_PRECONDITION, Some (PB.Result.show_error err))
    in
    let encode = compose Ocaml_protoc_plugin.Writer.contents encode in
    let methd = R.name' () in
    fun handler t ->
      let handler' ctx md raw =
        let req_time = Time.now () in
        let () =
          Lwt.async
          @@ fun () ->
          Logs_lwt.debug ~src:Log.default
          @@ fun m ->
          let tags = default_tags ctx in
          m ~tags ~header:Log.header "get request on %s" methd
        in
        let%lwt res = handler ctx md raw in
        let () =
          Lwt.async
          @@ fun () ->
          Logs_lwt.debug ~src:Log.default
          @@ fun m ->
          let diff = Time.(diff (now ()) req_time |> Span.to_string) in
          let tags = default_tags ctx |> Log.Tag.str ~name:"duration" diff in
          m ~tags "send response: duration %s" diff
        in
        Lwt.return res
      in
      Grpc_core.Server.add_handler
        t
        ~typ:`Unary
        ~methd
        ~unmarshall:decode
        ~marshall:encode
        handler';
      t
 ;;
end
