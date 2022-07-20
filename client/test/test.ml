module EchoService = Proto.Grpc_test.Echo

let greet () =
  let client = Grpc_client.make ~host:"loopback.ja-sore.de" ~port:50051 () in
  let req = EchoService.Greet.Request.make ~message:"hello" () in
  client.unary EchoService.greet' req
;;

let () =
  Lwt_main.run
  @@
  let () =
    let p = Lwt_process.(shell "yarn --offline node index.js" |> new process_none) in
    Lwt_main.at_exit (fun () -> Lwt.return @@ p#kill 9);
    Unix.sleep 2
  in
  greet ()
  |> Fun.flip Lwt.bind
     @@ function
     | Ok (msg, _) ->
       print_endline msg;
       Lwt.return_unit
     | Error (err, msg, _tr) ->
       let msg = Option.value msg ~default:"" in
       Printf.eprintf "%s: %s\n" (Grpc_core.Status.Code.show_fail_bwd err) msg;
       exit 1
;;
