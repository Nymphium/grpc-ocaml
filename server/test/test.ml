module Ctx = struct
  let request_id = Grpc_server.Context.Key.create ("request-id", Fun.id)
end

let handler =
  let module EchoService = Proto.Grpc_test.Echo in
  Grpc_server.Handler.(
    empty
    |> Unary.add EchoService.greet'
       @@ fun ctx message ->
       let request_id = Grpc_server.Context.get Ctx.request_id ctx in
       print_endline request_id;
       Lwt_result.return message)
;;

let middlewares =
  let open Grpc_server in
  let open Middlewares in
  empty
  |> add (fun ctx reqd ->
         let H2.Request.{ headers; _ } = H2.Reqd.request reqd in
         let request_id =
           Headers.get "request-id" headers
           |> (function
                | None -> Headers.get "x-request-id" headers
                | Some _ as some -> some)
           |> Option.value ~default:(Random.int 50000 |> Printf.sprintf "request-id-is-%d")
         in
         Context.add Ctx.request_id request_id ctx)
;;

let () =
  let host = "localhost" in
  let port = 20001 in
  let cmd =
    Lwt_process.(
      shell
      @@ Printf.sprintf
           {|grpcurl -vv -plaintext -import-path . -proto proto.proto -d '{ "message": "hi" }' %s:%d grpc_test.Echo/Greet|}
           host
           port)
  in
  Lwt_main.run
  @@
  let open Lwt.Syntax in
  let () =
    Lwt.(
      async
      @@ fun () ->
      print_endline "establish server...";
      Grpc_server.establish ~host ~port ~middlewares handler >|= Fun.const ())
  in
  print_endline "send gRPC via grpcurl...";
  let* st = Lwt_process.exec cmd in
  let* _ = Lwt_process.exec cmd in
  let* _ = Lwt_process.exec cmd in
  let* _ = Lwt_process.exec cmd in
  let st' =
    match st with
    | Unix.WEXITED st | Unix.WSIGNALED st | Unix.WSTOPPED st -> st
  in
  exit st'
;;
