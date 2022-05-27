open struct
  let make_grpc_handler context middlewares handlers =
    let ctx' = middlewares context in
    fun request reqd ->
      let { H2.Request.target; headers; _ } = request in
      let () =
        Lwt.async
        @@ fun () ->
        Logs_lwt.debug ~src:Log.src
        @@ fun m ->
        m
          "get request %s"
          target
          ~header:Log.header
          ~tags:Log.(Tag.empty |> Tag.upstream_header headers |> Tag.target target)
      in
      match Handler.Map.find_exn handlers target with
      | `Unary handler -> Rpc.unary handler (ctx' reqd) reqd
  ;;

  let invalid_req reqd =
    let msg = "no handler except for POST or non-gRPC content" in
    let () =
      Lwt.async
      @@ fun () ->
      Logs_lwt.debug ~src:Log.src
      @@ fun m ->
      m
        "%s"
        msg
        ~header:Log.header
        ~tags:Log.(Tag.empty |> Tag.request @@ H2.Reqd.request reqd)
    in
    let res = H2.Response.create `Bad_request in
    H2.Reqd.respond_with_string reqd res msg
  ;;

  let make_request_handler context middlewares handlers =
    let grpc_handler = make_grpc_handler context middlewares handlers in
    fun _client_addr reqd ->
      let (H2.Request.{ meth; target; headers; _ } as request) = H2.Reqd.request reqd in
      match meth with
      | `POST ->
        let is_grpc =
          Grpc_basic.Headers.Content_type.get headers
          |> Option.map @@ String.starts_with ~prefix:"application/grpc"
          |> Option.value ~default:false
        in
        (match target with
        | "/grpc.reflection.v1alpha.ServerReflection/ServerReflectionInfo" when is_grpc ->
          let msg = "reflection is not implemented" in
          let () =
            Lwt.async
            @@ fun () ->
            Logs_lwt.debug ~src:Log.src @@ fun m -> m "%s" msg ~header:Log.header
          in
          let res = H2.Response.create `Bad_request in
          H2.Reqd.respond_with_string reqd res msg
        | _ when is_grpc -> grpc_handler request reqd
        | _ -> invalid_req reqd)
      | _ -> invalid_req reqd
  ;;

  let error_handler _client_addr ?request:_ error start_response =
    let response_body = start_response Grpc_basic.Headers.empty in
    H2.Body.write_string
      response_body
      (match error with
      | `Exn exn -> Printexc.to_string exn
      | #H2.Status.standard as error -> H2.Status.default_reason_phrase error);
    H2.Body.close_writer response_body
  ;;
end

let establish ~host ~port ?(middlewares = Middlewares.empty) handlers =
  let context = Context.empty in
  let request_handler = make_request_handler context middlewares handlers in
  let connection_handler =
    H2_lwt_unix.Server.create_connection_handler ~request_handler ~error_handler
  in
  let addr =
    Unix.getaddrinfo host (string_of_int port) [ AI_CANONNAME; AI_FAMILY PF_INET ]
    |> List.hd
  in
  Lwt_io.establish_server_with_client_socket addr.ai_addr connection_handler
;;

let establish_and_run ~host ~port ?middlewares handlers =
  Lwt.map (Fun.const ()) @@ establish ~host ~port ?middlewares handlers
;;
