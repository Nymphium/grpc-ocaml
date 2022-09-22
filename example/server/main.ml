let server host port =
  Grpc_server.make_insecure ~host ~port ~interceptors:Interceptors.m Handler.h
;;

let () =
  let open Settings in
  let server = server host port in
  let () =
    Lwt.async
    @@ fun () ->
    let open Lwt.Syntax in
    let* () = Logs_lwt.debug (fun m -> m "run gRPC echo server on %s:%d ..." host port) in
    Grpc_server.start_with_handle_shutdown server
  in
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
;;
