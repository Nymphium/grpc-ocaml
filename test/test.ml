module EchoService = Proto.Grpc_test.Echo

let client host port =
  let client = Grpc_client.make ~host ~port () in
  let req = EchoService.Greet.Request.make ~message:"hello" () in
  client.unary EchoService.greet' req
;;

let server host port =
  let module Ctx = struct
    let request_id = Grpc_server.Context.Key.create ("request-id", Fun.id)
  end
  in
  let middlewares =
    let open Grpc_server in
    let open Middlewares in
    empty
    |> add (fun ctx reqd ->
           let H2.Request.{ headers; _ } = H2.Reqd.request reqd in
           let request_id =
             Headers.get "request-id" headers
             |> (function
                  | None -> Headers.get "x-request-id" headers
                  | Some _ as some -> some)
             |> Option.value
                  ~default:(Random.int 50000 |> Printf.sprintf "request-id-is-%d")
           in
           Context.add Ctx.request_id request_id ctx)
  in
  let handlers =
    Grpc_server.Handler.(
      empty
      |> Unary.add EchoService.greet'
         @@ fun ctx message ->
         let request_id = Grpc_server.Context.get Ctx.request_id ctx in
         print_endline request_id;
         Lwt_result.return message)
  in
  Grpc_server.establish_and_run ~host ~port ~middlewares handlers
;;

let () =
  let host = "localhost" in
  let port = 20000 in
  Lwt_main.run
  @@
  let open Lwt.Syntax in
  let () =
    Lwt.(
      async
      @@ fun () ->
      print_endline "establish server...";
      server host port)
  in
  let* res = client host port in
  let* _ = client host port in
  let* _ = client host port in
  let* _ = client host port in
  match res with
  | Ok (msg, _) ->
    print_endline msg;
    Lwt.return_unit
  | Error (err, msg) ->
    let msg = Option.value msg ~default:"" in
    Printf.eprintf "%s: %s\n" (Grpc_basic.Error.show err) msg;
    exit 1
;;
