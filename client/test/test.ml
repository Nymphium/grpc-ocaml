module EchoService = Proto.Grpc_test.Echo

let client = Grpc_client.make ~host:"0.0.0.0" ~port:50051 ()
let _unit = client.unary EchoService.unit' ()

let greet () =
  let req = EchoService.Greet.Request.make ~message:"hello" () in
  client.unary EchoService.greet' req
;;

let () =
  Lwt_main.run
  @@
  let open Lwt.Syntax in
  let p = Lwt_process.(shell "node index.js" |> open_process) in
  let* () = Lwt_unix.sleep 5. in
  let* res = greet () in
  let _ = p#terminate in
  match res with
  | Ok (msg, _) ->
    print_endline msg;
    Lwt.return_unit
  | Error (err, msg, _tr) ->
    let msg = Option.value msg ~default:"" in
    Printf.eprintf "%s: %s\n" (Grpc_core.Status.Code.show_fail_bwd err) msg;
    exit 1
;;
