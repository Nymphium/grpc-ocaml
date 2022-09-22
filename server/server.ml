let start = Grpc_core.Server.start
let shutdown = Grpc_core.Server.shutdown
let add_host = Grpc_core.Server.add_host

let make_insecure
    ~host
    ~port
    ?(args = [])
    ?(interceptors = Grpc_core.Server.Interceptors.empty)
    handlers
  =
  let t = Grpc_core.Server.make args interceptors in
  add_host ~host ~port t;
  handlers t
;;

(** start server and attach shutdown hook at the end of Lwt main loop, IO cancelation, and POSIX signals specified with [signals] (sigint by default) *)
let start_with_handle_shutdown ?(signals = [ Sys.sigint ]) t =
  let io = start t in
  let () = Lwt_main.at_exit (fun () -> shutdown t) in
  let _ =
    List.iter
      (fun signal ->
        ignore
        @@ Lwt_unix.on_signal signal (fun _ ->
               Lwt.cancel io;
               exit signal))
      signals
  in
  let () =
    Lwt.on_cancel io (fun () -> Lwt_preemptive.run_in_main @@ fun () -> shutdown t)
  in
  io
;;
