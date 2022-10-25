module EchoService = Proto.Grpc_test.Echo

let client = Grpc_client.make ~host:"0.0.0.0" ~port:50051 ()

let greet () =
  let req = EchoService.Greet.Request.make ~message:"hello" () in
  client.unary EchoService.greet' req
;;

let () =
  Lwt_main.run
  @@
  let open Lwt.Syntax in
  let _ = Lwt_process.(shell "node index.js" |> exec) in
  let* () = Lwt_unix.sleep 5. in
  let* res = greet () in
  match res with
  | Ok (msg, _) ->
    print_endline msg;
    Lwt.return_unit
  | Error (err, msg) ->
    let msg = Option.value msg ~default:"" in
    Printf.eprintf "%s: %s\n" (Grpc_basic.Error.show err) msg;
    exit 1
;;
