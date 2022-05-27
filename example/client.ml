module EchoService = Proto.Grpc_test.Echo

let send_greet host port =
  let client = Grpc_client.make ~host ~port () in
  let req = EchoService.Greet.Request.make ~message:"hello" () in
  client.unary EchoService.greet' req
;;

let () =
  let open Settings in
  let () = Lazy.force log_init in
  Lwt_main.run
  @@
  let open Lwt.Syntax in
  let* res = send_greet host port in
  match res with
  | Ok (msg, _) ->
    Logs.debug (fun m -> m "response: %s" msg);
    Lwt.return_unit
  | Error (err, msg) ->
    let msg = Option.value msg ~default:"" in
    Printf.eprintf "%s: %s\n" (Grpc_basic.Error.show err) msg;
    exit 1
;;
