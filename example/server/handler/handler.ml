module EchoService = Proto.Grpc_test.Echo

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
           let@ () = Logs_lwt.debug (fun m -> m "request-id: %s" request_id) in
           (* let@ () = Lwt_unix.sleep 0.1 in *)
           let metadata = [ "x-response", "ok" ] in
           let@ () = Lwt_io.printlf "%d" @@ Grpc_server.Context.get Context.counter ctx in
           ok' ~metadata message)
    |> Unary.add EchoService.unit' (fun _ _ _ _ -> ok' ()))
;;
