open Grpc_core
open Grpc_basic
open Core
open Fn

open struct
  module PB = Ocaml_protoc_plugin

  let encode encoder = compose encoder PB.Reader.create
end

type t =
  { channel : Channel.t
  ; unary : 'req 'res. ('req, 'res) unary
  }

and ('req, 'res) unary =
  (module Ocaml_protoc_plugin.Service.Rpc
     with type Request.t = 'req
      and type Response.t = 'res)
  -> ?timeout:int
  -> ?request_id:string
  -> ?metadata:Metadata.bwd
  -> 'req
  -> 'res Protoiso.res Lwt.t

(**
 [make service_addr service'] makes gRPC client.
 @param ?timeout: optional int as timeout (second)
 @param ?request_id: optional string as "x-request-id" for tracking forwarding request
 *)
let make ~host ~port ?credentials ?(args = []) () =
  let addr = Printf.sprintf "%s:%d" host port in
  let channel = Channel.make ?credentials addr args in
  let unary : type req res. (req, res) unary =
   fun rpc' ?(timeout = 10) ?request_id ->
    let path =
      let module S = (val rpc') in
      S.name' ()
    in
    let decoder, encoder = PB.Service.make_client_functions' rpc' in
    let set_request_id =
      Option.value_map request_id ~default:id ~f:(fun id -> Headers.add "request_id" id)
    in
    fun ?(metadata = []) req ->
      let metadata = Headers.(metadata |> Timeout.set_second timeout) |> set_request_id in
      let body = PB.Writer.contents @@ decoder req in
      let call = Call.make ~channel ~methd:path () in
      Call.unary_request call ~metadata ~message:body
      |> flip Lwt_result.bind
         @@ fun (body, trailers) ->
         Lwt.return
         @@
         match encode encoder (Option.value ~default:"" body) with
         | Ok res -> Ok (res, trailers)
         | Error err ->
           Error (`FAILED_PRECONDITION, Some (PB.Result.show_error err), set_request_id [])
  in
  { channel; unary }
;;
