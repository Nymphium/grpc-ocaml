type wrapped_server =
  { it : unit Ctypes.ptr
  ; mutable queue : unit Ctypes.ptr
  }

type call_stack =
  { call : unit Ctypes.ptr
  ; details : Ffi.Type.Call_details.t Ctypes.structure Ctypes.ptr
  ; request_metadata : Ffi.Type.Metadata.Array.t Ctypes.structure Ctypes.ptr
  }

let new_call_stack () =
  { call = Ctypes.null
  ; details = Ctypes.(addr @@ make Ffi.Type.Call_details.t)
  ; request_metadata = Ctypes.(addr @@ make Ffi.Type.Metadata.Array.t)
  }
;;

let run_server () =
  let () =
    Lwt.async
    @@ fun () ->
    let open Lwt.Syntax in
    let* () = (Ffi.init ()).lwt in
    let* server =
      (Ffi.Server.create None Ctypes.null).lwt
      |> Lwt.map (fun it -> { it; queue = Ctypes.null })
    in
    let* creds = (Ffi.Server.Credentials.create_insecure ()).lwt in
    let () =
      Inspect.Sexpr.dump creds;
      flush stdout
    in
    print_endline "hoge";
    let* cq = (Ffi.Completion_queue.create_for_pluck Ctypes.null).lwt in
    let* () = (Ffi.Server.register_completion_queue server.it cq Ctypes.null).lwt in
    (* XXX: segfault by uncommening the below *)
    (* let () = server.queue <- cq in *)
    let* _ok = (Ffi.Server.add_http2_port server.it "localhost:50051" creds).lwt in
    let* _failure =
      (Ffi.Server.register_method
         server.it
         "grpc_test.Echo/Greet"
         "hoge"
         Ffi.Type.Server.Register_method_payload_handling.NONE
         (Unsigned.UInt32.of_int 0))
        .lwt
    in
    let* _req_error =
      let* call_queue = (Ffi.Completion_queue.create_for_pluck Ctypes.null).lwt in
      (* let bq = Ffi.Type.Server. *)
      let cs = new_call_stack () in
      (Ffi.Server.request_call
         server.it
         cs.call
         cs.details
         cs.request_metadata
         call_queue
         server.queue
         Ctypes.null)
        .lwt
    in
    (Ffi.Server.start server.it).lwt
  in
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
;;

(* let run_client () = *)
(* Lwt_main.run *)
(* @@ *)
(* let open Lwt.Syntax in *)
(* let* () = (Ffi.init ()).lwt in *)
(* let* creds = (Ffi.Channel.Credentials.create_insecure ()).lwt in *)
(* let* ch = (Ffi.Channel.create "localhost:50080" creds None).lwt in *)
(* let* st = (Ffi.Channel.Connectivity.State.check ch 1).lwt in *)
(* let () = *)
(* Inspect.Sexpr.dump st; *)
(* flush stdout *)
(* in *)
(* Lwt.return_unit *)
(* ;; *)

let () =
  (* ignore run_server; *)
  run_server ()
;;
