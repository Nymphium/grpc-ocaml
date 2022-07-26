let unbox = Ffi.unbox

(* let run_server () = *)
(* let () = *)
(* Lwt.async *)
(* @@ fun () -> *)
(* let open Lwt.Syntax in *)
(* let* () = unbox @@ Ffi.init () in *)
(* let* server = unbox @@ Ffi.Server.create None Ctypes.null in *)
(* let* creds = unbox @@ Ffi.Server.Credentials.create_insecure () in *)
(* let () = *)
(* Inspect.Sexpr.dump creds; *)
(* flush stdout *)
(* in *)
(* print_endline "hoge"; *)
(* let* cq = unbox @@ Ffi.Completion_queue.create_for_pluck Ctypes.null in *)
(* let* () = unbox @@ Ffi.Server.register_completion_queue server cq Ctypes.null in *)
(* let* _ok = unbox @@ Ffi.Server.add_http2_port server "localhost:50051" creds in *)
(* let* _failure = *)
(* unbox *)
(* @@ Ffi.Server.register_method *)
(* server *)
(* "grpc_test.Echo/Greet" *)
(* "hoge" *)
(* Ffi.Type.Server.Register_method_payload_handling.NONE *)
(* (Unsigned.UInt32.of_int 0) *)
(* in *)
(* let _req_error = *)
(* let* call_queue = unbox @@ Ffi.Completion_queue.create_for_pluck Ctypes.null in *)
(* let call = Ctypes.null in *)
(* let details = Ctypes.(addr @@ make Ffi.Type.Call_details.t) in *)
(* let* () = unbox @@ Ffi.Call.Details.init details in *)
(* let request_metadata = Ctypes.(addr @@ make Ffi.Type.Metadata.Array.t) in *)
(* let* () = unbox @@ Ffi.Metadata.init request_metadata in *)
(* let tag = Ctypes.(coerce void (ptr void) ()) in *)
(* unbox *)
(* @@ Ffi.Server.request_call server call details request_metadata call_queue cq tag *)
(* in *)
(* (Ffi.Server.start server).lwt *)
(* in *)
(* let forever, _ = Lwt.wait () in *)
(* Lwt_main.at_exit (fun () -> (Ffi.shutdown ()).lwt); *)
(* Lwt_main.run forever *)
(* ;; *)

(* let () = run_server () *)
(* Fun.protect ~finally:(fun () -> Ffi.shutdown ()) @@ fun () -> ignore @@ run_server () *)

let run_client () =
  Lwt_main.at_exit (fun () -> unbox @@ Ffi.shutdown ());
  Lwt_main.run
  @@
  let open Lwt.Syntax in
  let* () = unbox @@ Ffi.init () in
  let* creds = unbox @@ Ffi.Channel.Credentials.create_insecure () in
  let* ch = unbox @@ Ffi.Channel.create "localhost:50080" creds None in
  let* cq = unbox @@ Ffi.Completion_queue.create_for_pluck Ctypes.null in
  let* methd = unbox @@ Ffi.Slice.from_static_string "tabun invalid " in
  let* host = Ffi.Slice.from_static_string "its-me" |> unbox |> Lwt.map Ctypes.addr in
  let* deadline = unbox @@ Ffi.Timespec.inf_future MONOTONIC in
  let tags = Ctypes.null in
  let* call =
    unbox
    @@ Ffi.Channel.create_call
         ch
         Ctypes.null
         (Unsigned.UInt32.of_int 0)
         cq
         methd
         host
         deadline
         tags
  in
  let* v = unbox @@ Ffi.Call.get_peer call in
  print_endline v;
  let op = Ctypes.addr @@ Ctypes.make Ffi.Type.Op.t in
  let* er =
    unbox @@ Ffi.Call.start_batch call op (Unsigned.Size_t.of_int 0) tags Ctypes.null
  in
  Ffi.Call.error_to_string er |> unbox |> Fun.flip Lwt.bind Lwt_io.print
;;

let () =
  (* Fun.protect ~finally:(fun () -> Ffi.shutdown ()) @@ fun () -> ignore @@ *)
  run_client ()
;;
