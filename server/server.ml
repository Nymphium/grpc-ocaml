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

let establish ~host ~port ?args ?middlewares handlers =
  let t = make_insecure ~host ~port ?args ?middlewares handlers in
  Lwt_main.at_exit (fun () -> Grpc_core.Server.shutdown t);
  Grpc_core.Server.start t
;;
