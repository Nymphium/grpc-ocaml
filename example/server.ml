module EchoService = Proto.Grpc_test.Echo

let server host port =
  let module Ctx = struct
    let request_id = Grpc_server.Context.Key.create ("request-id", Fun.id)
  end
  in
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
             |> Option.value
                  ~default:(Random.int 50000 |> Printf.sprintf "request-id-is-%d")
           in
           Context.add Ctx.request_id request_id ctx)
  in
  let handlers =
    Grpc_server.Handler.(
      empty
      |> Unary.add EchoService.greet'
         @@ fun ctx message ->
         let request_id = Grpc_server.Context.get Ctx.request_id ctx in
         Logs.debug (fun m -> m "request-id: %s" request_id);
         Lwt_result.return message)
  in
  Grpc_server.establish_and_run ~host ~port ~middlewares handlers
;;

let () =
  let open Settings in
  Lazy.force log_init;
  let () =
    Lwt.(
      async
      @@ fun () ->
      Logs.debug (fun m -> m "run gRPC echo server on %s:%d ..." host port);
      server host port)
  in
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
;;
