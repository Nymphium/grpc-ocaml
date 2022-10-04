module EchoService = Proto.Grpc_test.Echo

let client = Grpc_client.make ~host:"0.0.0.0" ~port:20000 ()

let h t =
  Grpc_server.Handler.(
    let open Grpc_basic.Syntax in
    t
    |> Unary.add EchoService.greet' (fun ctx headers _ message ->
           let request_id = Grpc_server.Context.get Context.request_id ctx in
           let a = Grpc_basic.Headers.get "x-foo" headers in
           let@ () =
             Logs_lwt.debug (fun m -> m "x-foo: %s" (Option.value ~default:"<empty>" a))
           in
           let* _res, _tr = client.Grpc_client.unary EchoService.greet' "Hi" in
           let@ () = Logs_lwt.debug (fun m -> m "request-id: %s" request_id) in
           let metadata = [ "x-response", "ok" ] in
           let@ () = Lwt_io.printlf "%d" @@ Grpc_server.Context.get Context.counter ctx in
           ok' ~metadata message)
    |> Unary.add EchoService.unit' (fun _ _ _ _ -> ok' ()))
;;
