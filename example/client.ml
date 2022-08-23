module EchoService = Proto.Grpc_test.Echo

let client =
  let open Settings in
  Grpc_client.make ~host ~port ()
;;

let send_greet client =
  let req = EchoService.Greet.Request.make ~message:"hello" () in
  client.Grpc_client.unary EchoService.greet' ~timeout:1 req
;;

let () =
  Lwt_main.run
  @@
  let open Lwt.Syntax in
  (* let* _res = send_greet client in *)
  let* res = send_greet client in
  (* let _res = send_greet client in *)
  (* let _res = send_greet client in *)
  (* let _res = send_greet client in *)
  match res with
  | Ok (msg, _) ->
    let* () = Logs_lwt.debug (fun m -> m "response: %s" msg) in
    Lwt.return_unit
  | Error (err, msg, _) ->
    let msg = Option.value msg ~default:"" in
    Printf.eprintf "%s: %s\n" (Grpc_basic.Error.show err) msg;
    exit 1
;;
