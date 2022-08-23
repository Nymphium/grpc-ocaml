module EchoService = Proto.Grpc_test.Echo

let server host port =
  let module Ctx = struct
    let request_id = Grpc_server.Context.Key.create ("request-id", Fun.id)
  end
  in
  let middlewares =
    let open Grpc_server in
    let open Middlewares in
    empty
    |> add (fun ctx headers _raw_data ->
           let request_id =
             List.assoc_opt "request-id" headers
             |> (function
                  | None -> List.assoc_opt "x-request-id" headers
                  | Some _ as some -> some)
             |> Option.value
                  ~default:(Random.int 50000 |> Printf.sprintf "request-id-is-%d")
           in
           Context.add Ctx.request_id request_id ctx)
  in
  let handlers =
    Grpc_server.Handler.(
      let open Grpc_basic.Syntax in
      Unary.add EchoService.greet'
      @@ fun ctx headers message ->
      let request_id = Grpc_server.Context.get Ctx.request_id ctx in
      let a = Grpc_basic.Headers.get "x-foo" headers in
      let@ () =
        Logs_lwt.debug (fun m -> m "x-foo: %s" (Option.value ~default:"<empty>" a))
      in
      let@ () = Logs_lwt.debug (fun m -> m "request-id: %s" request_id) in
      (* let metadata = [ "x-response", "ok" ] in *)
      (* let@ () = Lwt_unix.sleep 2. in *)
      ok' message)
  in
  Grpc_server.make_insecure ~host ~port ~middlewares handlers
;;

let () =
  let open Settings in
  let server = server host port in
  let () =
    Lwt.async
    @@ fun () ->
    let open Lwt.Syntax in
    let* () = Logs_lwt.debug (fun m -> m "run gRPC echo server on %s:%d ..." host port) in
    Grpc_server.start_with_handle_shutdown server
  in
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
;;
