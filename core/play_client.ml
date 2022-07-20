let string_of_md tr =
  List.map (fun (k, v) -> Printf.sprintf {|{"%s": "%s"}|} k v) tr |> String.concat ", "
;;

let () =
  let () = Lwt_main.at_exit Grpc_core.Top.shutdown in
  Lwt_main.run
  @@
  let io () =
    let open Lwt.Syntax in
    let ch = Grpc_core.Channel.make "localhost:21090" [] in
    let* () =
      let target = Grpc_core.Channel.get_target ch in
      Lwt_io.printf "\n\n\n>>> send message to %s\n\n\n" target
    in
    let call = Grpc_core.Call.make ~channel:ch ~methd:"/grpc_test.Echo/Geet" () in
    let* () = Lwt_io.printl "send" in
    let res =
      Grpc_core.Call.unary_request
        ~metadata:[ "x-md-msg", "hoge" ]
        ~message:"\x00\x00\x00\x00\x07\x0a\x05hello"
        call
    in
    let* () = Lwt_io.printl "wait" in
    let* () = Lwt_unix.sleep 1. in
    let* () = Lwt_io.printl "wait" in
    let* () = Lwt_unix.sleep 1. in
    let* res in
    match res with
    | `Ok (msg, md) ->
      Lwt_io.printlf
        {|{ status:"ok"; msg: "%s"; metadata: [%s] }|}
        (Option.value ~default:"<none>" msg)
      @@ string_of_md md
    | `Error (st, details, md) ->
      Lwt_io.printlf
        {|{ status: "%s"; details: "%s"; metadata: [%s] }|}
        (Grpc_core.Status.Code.show st)
        (Option.value ~default:"" details)
        (string_of_md md)
  in
  List.init 1 (fun _ -> io ()) |> Lwt.all |> Lwt.map ignore
;;
(* |> Fun.flip Lwt.bind (fun _ -> Lwt_unix.sleep 5.) *)
