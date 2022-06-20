exception Connection_error of H2.Client_connection.error

type t = H2_lwt_unix.Client.t Lwt_pool.t

open struct
  let validate conn = Lwt.return @@ not @@ H2_lwt_unix.Client.is_closed conn

  let dispose conn =
    let () =
      Lwt.(
        async
        @@ fun () ->
        Logs_lwt.debug ~src:Log.src @@ fun m -> m ~header:Log.header "disconnect")
    in
    H2_lwt_unix.Client.shutdown conn
  ;;

  let set_ping_poll ping_duration ping_times conn =
    let dispose () =
      (* randomly sleep to avoid all connections to shutdonwn simutaneously *)
      Lwt.(Core.Random.float 10. |> Lwt_unix.sleep >>= fun () -> dispose conn)
    in
    Lwt.async
    @@ fun () ->
    try%lwt
      let%lwt () =
        for%lwt i = 0 to ping_times do
          let pong, pong_notify = Lwt.wait () in
          H2_lwt_unix.Client.ping conn (Lwt.wakeup pong_notify);
          (* PING must be returned within 1sec *)
          let%lwt () = Lwt_unix.with_timeout 1. (Fun.const pong) in
          Lwt_unix.sleep ping_duration
        done
      in
      dispose ()
    with
    | Lwt_unix.Timeout -> dispose ()
  ;;

  let sock_domain = Unix.PF_INET

  let allocate_socket ~host ~port =
    let port = string_of_int port in
    let%lwt addrs =
      Lwt_unix.getaddrinfo host port [ AI_CANONNAME; AI_FAMILY sock_domain ]
    in
    match addrs with
    | [] -> Lwt.fail_with @@ Printf.sprintf "failed to resolve hostname: %s:%s" host port
    | _ ->
      let%lwt msocket =
        (* try connecting with addr *)
        Lwt_list.fold_left_s
          (fun addr_ok addr_check ->
            match addr_ok with
            | None ->
              let socket =
                let it = Lwt_unix.socket sock_domain Unix.SOCK_STREAM 0 in
                let () = Lwt_unix.setsockopt it Unix.SO_KEEPALIVE true in
                let () = Lwt_unix.setsockopt it Unix.SO_REUSEADDR true in
                it
              in
              (try%lwt
                 let%lwt () = Lwt_unix.connect socket addr_check.Unix.ai_addr in
                 Lwt.return @@ Some socket
               with
              | _ ->
                let () =
                  Lwt.async
                  @@ fun () ->
                  Logs_lwt.debug ~src:Log.src (fun m ->
                      m ~header:Log.header "failed to use fd")
                in
                let%lwt () = Lwt_unix.close socket in
                Lwt.return @@ None)
            | addr -> Lwt.return @@ addr)
          None
          addrs
      in
      (match msocket with
      | Some socket -> Lwt.return socket
      | None -> Lwt.fail_with @@ Printf.sprintf "no available address for %s:%s" host port)
  ;;

  let make_connection host port =
    let%lwt socket = allocate_socket ~host ~port in
    let () =
      Lwt.(
        async
        @@ fun () ->
        Logs_lwt.debug ~src:Log.src @@ fun m -> m ~header:Log.header "connect")
    in
    let connection_error, notify_connection_error = Lwt.wait () in
    let error_handler = Lwt.wakeup_later notify_connection_error in
    Lwt.(
      pick
        [ H2_lwt_unix.Client.create_connection ~error_handler socket >|= Result.ok
        ; connection_error >|= Result.error
        ])
  ;;
end

(** [make ?ping_duration ?ping_times error_handler ~host ~port ~pool_size] makes a connection pool with sized [pool_size]
Check connection by sending PING frame with duration [ping_duration] second by [ping_times] times.
*)
let make ?(ping_duration = 360.) ?(ping_times = 10) ~host ~port ~pool_size () : t =
  let pool =
    Lwt_pool.create pool_size ~validate ~dispose
    @@ fun () ->
    let%lwt conn = make_connection host port in
    match conn with
    | Ok conn ->
      let () = set_ping_poll ping_duration ping_times conn in
      Lwt.return conn
    | Error err -> Lwt.fail @@ Connection_error err
  in
  let () = ignore @@ Lwt_main.Exit_hooks.add_first @@ fun () -> Lwt_pool.clear pool in
  pool
;;
