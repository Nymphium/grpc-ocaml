module Ctx = struct
  let request_id = Grpc_server.Context.Key.create ("request-id", Fun.id)
end

let handler =
  let module EchoService = Proto.Grpc_test.Echo in
  Grpc_server.Handler.(
    Unary.add EchoService.greet'
    @@ fun ctx _headers message ->
    let open Grpc_basic.Syntax in
    let request_id = Grpc_server.Context.get Ctx.request_id ctx in
    let@ () = Logs_lwt.debug @@ fun m -> m "%s" request_id in
    ok' message)
;;

let middlewares =
  let open Grpc_server in
  let open Middlewares in
  empty
  |> add (fun ctx _data headers ->
         let request_id =
           List.assoc_opt "request-id" headers
           |> (function
                | None -> List.assoc_opt "x-request-id" headers
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
  let server = Grpc_server.make_insecure ~host ~port ~middlewares handler in
  let () =
    Lwt.(
      async
      @@ fun () ->
      let* () = Logs_lwt.debug @@ fun m -> m "establish server" in
      Grpc_server.start_with_handle_shutdown server)
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
