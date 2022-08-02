(* open Stub *)

(*let () =*)
(*Lwt_main.run*)
(*@@*)
(*let open Lwt.Syntax in*)
(*let* bb = Grpc_core.Byte_buffer.from_ocaml_string "a" in*)
(*let bb2 = Ctypes.(!@bb) in*)
(*let () =*)
(*print_endline @@ Grpc_core.Byte_buffer.inspect bb2;*)
(*flush stdout*)
(*in*)
(*let* s = Grpc_core.Byte_buffer.to_ocaml_string bb in*)
(*let l = Option.fold s ~none:"damedayo~" ~some:Fun.id in*)
(*Lwt_io.printl l*)
(*;;*)

(* let* slice = unbox @@ Grpc_core.Import.F.Slice.from_static_string "aaa" in *)
(* let a = Grpc_core.Slice.start_ptr slice in *)
(* let () = *)
(* Inspect.Sexpr.dump a; *)
(* flush stdout *)
(* in *)
(* print_endline @@ Grpc_core.Slice.to_ocaml_string slice; *)

(* let str = Grpc_core.Slice.to_ocaml_string slice in *)
(* Lwt_io.printl str *)

(* let run_server () = *)
(* let () = *)
(* Lwt.async *)
(* @@ fun () -> *)
(* let open Lwt.Syntax in *)
(* let* () = unbox @@ Stub.init () in *)
(* let* server = unbox @@ Stub.Server.create None Ctypes.null in *)
(* let* creds = unbox @@ Stub.Server.Credentials.create_insecure () in *)
(* let () = *)
(* Inspect.Sexpr.dump creds; *)
(* flush stdout *)
(* in *)
(* print_endline "hoge"; *)
(* let* cq = unbox @@ Stub.Completion_queue.create_for_pluck Ctypes.null in *)
(* let* () = unbox @@ Stub.Server.register_completion_queue server cq Ctypes.null in *)
(* let* _ok = unbox @@ Stub.Server.add_http2_port server "localhost:50051" creds in *)
(* let* _failure = *)
(* unbox *)
(* @@ Stub.Server.register_method *)
(* server *)
(* "grpc_test.Echo/Greet" *)
(* "hoge" *)
(* Stub.Type.Server.Register_method_payload_handling.NONE *)
(* (Unsigned.UInt32.of_int 0) *)
(* in *)
(* let _req_error = *)
(* let* call_queue = unbox @@ Stub.Completion_queue.create_for_pluck Ctypes.null in *)
(* let call = Ctypes.null in *)
(* let details = Ctypes.(addr @@ make Stub.Type.Call_details.t) in *)
(* let* () = unbox @@ Stub.Call.Details.init details in *)
(* let request_metadata = Ctypes.(addr @@ make Stub.Type.Metadata.Array.t) in *)
(* let* () = unbox @@ Stub.Metadata.init request_metadata in *)
(* let tag = Ctypes.(coerce void (ptr void) ()) in *)
(* unbox *)
(* @@ Stub.Server.request_call server call details request_metadata call_queue cq tag *)
(* in *)
(* (Stub.Server.start server).lwt *)
(* in *)
(* let forever, _ = Lwt.wait () in *)
(* Lwt_main.at_exit (fun () -> (Stub.shutdown ()).lwt); *)
(* Lwt_main.run forever *)
(* ;; *)

(* let () = run_server () *)
(* Fun.protect ~finally:(fun () -> Stub.shutdown ()) @@ fun () -> ignore @@ run_server () *)

(* let run_client () = *)
(* Lwt_main.at_exit (fun () -> unbox @@ Stub.shutdown ()); *)
(* Lwt_main.run *)
(* @@ *)
(* let open Lwt.Syntax in *)
(* let* () = unbox @@ Stub.init () in *)
(* let* creds = unbox @@ Stub.Channel.Credentials.create_insecure () in *)
(* let* ch = unbox @@ Stub.Channel.create "localhost:50080" creds None in *)
(* let* cq = unbox @@ Stub.Completion_queue.create_for_pluck Ctypes.null in *)
(* let* methd = unbox @@ Stub.Slice.from_static_string "tabun invalid " in *)
(* let* host = Stub.Slice.from_static_string "its-me" |> unbox |> Lwt.map Ctypes.addr in *)
(* let* deadline = unbox @@ Stub.Timespec.inf_future MONOTONIC in *)
(* let tags = Ctypes.null in *)
(* let* call = *)
(* unbox *)
(* @@ Stub.Channel.create_call *)
(* ch *)
(* Ctypes.null *)
(* (Unsigned.UInt32.of_int 0) *)
(* cq *)
(* methd *)
(* host *)
(* deadline *)
(* tags *)
(* in *)
(* let* v = unbox @@ Stub.Call.get_peer call in *)
(* print_endline v; *)
(* let op = Ctypes.addr @@ Ctypes.make Stub.Type.Op.t in *)
(* let* er = *)
(* unbox @@ Stub.Call.start_batch call op (Unsigned.Size_t.of_int 0) tags Ctypes.null *)
(* in *)
(* Stub.Call.error_to_string er |> unbox |> Fun.flip Lwt.bind Lwt_io.print *)
(* ;; *)

(* let () = *)
(* (* Fun.protect ~finally:(fun () -> Stub.shutdown ()) @@ fun () -> ignore @@ *) *)
(* run_client () *)
(* ;; *)

let () =
  let () = Lwt_main.at_exit Grpc_core.Top.shutdown in
  Lwt_main.run
  (*@@ Lwt.finalize Grpc_core.Top.shutdown*)
  (*@@ fun () ->*)
  @@
  let open Lwt.Syntax in
  let* ch = Grpc_core.Channel.make "localhost:21090" [] in
  let* target = Grpc_core.Channel.get_target ch in
  let* call = Grpc_core.Channel.make_call ~channel:ch ~methd:"/grpc_test.Echo/Greet" () in
  let* () =
    let* st = Grpc_core.Slice.from_static_string "hoge" in
    let* len = Grpc_core.Slice.length st in
    let* st' = Grpc_core.Slice.to_ocaml_string st in
    let* () = Grpc_core.Slice.unref st in
    Lwt_io.printlf "len: %d; st: %s" len st'
  in
  let* () = Lwt_io.printf "\n\n\n>>> send message to %s\n\n\n" target in
  let* { recv_initial_metadata = metadata
       ; recv_message
       ; recv_status_on_client = { error; status; metadata = tr; details }
       }
    =
    Grpc_core.Call.request ~metadata:[ "x-md-msg", "hoge" ] ~message:"hello" call
  in
  let* () =
    let status = Stub.T.Status_code.show status in
    let error = Option.value ~default:"" error in
    let details = Option.value ~default:"" details in
    let tr =
      List.map (fun (k, v) -> Printf.sprintf "{%s: %s}" k v) tr |> String.concat ", "
    in
    Lwt_io.printlf
      "{ status: %s; error: %s; details: %s; tr: %s }"
      status
      error
      details
      tr
  in
  let () =
    Inspect.Sexpr.dump recv_message;
    (* Inspect.Sexpr.dump recv_status_on_client; *)
    Inspect.Sexpr.dump metadata;
    (* Inspect.Sexpr.dump t; *)
    flush stdout
  in
  Lwt.return_unit
;;
