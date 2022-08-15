let make_insecure
    ~host
    ~port
    ?(args = [])
    ?(middlewares = Grpc_core.Server.Middlewares.empty)
    handlers
  =
  let t = Grpc_core.Server.make args middlewares in
  Grpc_core.Server.add_host ~host ~port t;
  handlers t
;;

let start = Grpc_core.Server.start
let shutdown = Grpc_core.Server.shutdown

let start_with_handle_shutdown t =
  let () = Lwt_main.at_exit (fun () -> shutdown t) in
  start t
;;
