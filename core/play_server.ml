open Grpc_core
module EchoService = Proto.Grpc_test.Echo

let string_of_md tr =
  List.map (fun (k, v) -> Printf.sprintf {|{"%s": "%s"}|} k v) tr |> String.concat ", "
;;

let () =
  let () = Top.init () in
  (* Grpc_core.Completion_queue.tic := `Millis 300L; *)
  (* let () = *)
  (* Log.set_log_callback *)
  (* @@ fun ~file ~line ~log_level ~message -> *)
  (* if log_level = `Debug then Printf.printf "%s %d %s" file line message *)
  (* in *)
  let server =
    let request_id = Context.create_key "request_id" in
    let middlewares =
      Server.Middlewares.(
        empty
        |> add
           @@ fun ctx md _ ->
           let ctx' = Context.add request_id "1234" ctx in
           print_endline
           @@ Printf.sprintf ">>>> >>>>>>>>>>>>>>>>>>>>>>>>>>>> %s"
           @@ string_of_md md;
           ctx')
    in
    let s = Server.make [] middlewares in
    Server.add_host s ~host:"localhost" ~port:21090;
    let () =
      Server.add_handler
        s
        ~typ:`Unary
        ~methd:(EchoService.Greet.name' ())
        ~unmarshall:Result.ok
        ~marshall:Fun.id
      @@ fun ctx md s ->
      let s' =
        String.to_seq s
        |> List.of_seq
        |> List.map (fun c -> Printf.sprintf "0x%x" @@ Char.code c)
        |> String.concat " "
      in
      let open Lwt.Syntax in
      (* let* () = Lwt_unix.sleep 10. in *)
      let* () = Lwt_io.print (string_of_md md) in
      let* () = Lwt_io.printlf "request_id is %s" @@ Context.get request_id ctx in
      let* () = Lwt_io.printlf "hello, %s!" s' in
      Lwt.return @@ Response.success s ()
    in
    s
  in
  let () = Lwt_main.at_exit @@ Grpc_core.Top.shutdown in
  let () = Lwt.async @@ fun () -> Server.start server in
  Lwt_main.run
  @@
  let open Lwt.Syntax in
  let is_running = if Server.is_running server then "running" else "not started" in
  let* () = Lwt_io.printlf "%s" is_running in
  let io, _ = Lwt.wait () in
  io
;;
(* let* () = Lwt_unix.sleep 3. in *)
(* Server.shutdown server *)
