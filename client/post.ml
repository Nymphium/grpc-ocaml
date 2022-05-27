open Core

type t =
  { body : Buffer.t
  ; mutable headers : Grpc_basic.Headers.t
  ; mutable trailers : Grpc_basic.Headers.t
  ; mutable status : H2.Status.t
  ; mutable error : string Option.t
  }

let show_error = function
  | `Malformed_response s -> Printf.sprintf "`Malformed_response(%s)" s
  | `Invalid_response_body_length res ->
    let s = H2.Response.pp_hum Format.str_formatter res |> Format.flush_str_formatter in
    Printf.sprintf "`Invalid_response_body_length(%s)" s
  | `Protocol_error (code, e) ->
    Printf.sprintf "`Protocol_error(%s, %s)" (H2.Error_code.to_string code) e
  | `Exn exn -> Printf.sprintf "`Exn(%s)" @@ Exn.to_string exn
;;

let parse_uri uri =
  let uri' = Uri.to_string uri in
  let throw part = function
    | Some v -> v
    | None -> failwith @@ Printf.sprintf "failed to parse %s (from %s)" part uri'
  in
  let scheme = Uri.scheme uri |> throw "scheme" in
  let host = Uri.host uri |> throw "host" in
  let port = Uri.port uri |> throw "port" in
  let path = Uri.path uri in
  scheme, host, port, path
;;

let init () =
  { body = Buffer.create 128
  ; headers = Grpc_basic.Headers.empty
  ; trailers = Grpc_basic.Headers.empty
  ; status = `Internal_server_error
  ; error = None
  }
;;

let make_error_handler t error = t.error <- Some (show_error error)

let make_response_handler =
  let read_response response_body notify_response_received buf =
    let on_eof () =
      Lwt.wakeup_later notify_response_received ();
      H2.Body.close_reader response_body
    in
    let rec go () =
      H2.Body.schedule_read
        response_body
        ~on_read:(fun bigstring ~off ~len ->
          let response_fragment = Bytes.create len in
          let () =
            Bigstringaf.blit_to_bytes
              bigstring
              ~src_off:off
              response_fragment
              ~dst_off:0
              ~len
          in
          Buffer.add_bytes buf response_fragment |> go)
        ~on_eof
    in
    go ()
  in
  fun t notify_response_received response response_body ->
    let () = t.headers <- response.H2.Response.headers in
    let () = t.status <- response.status in
    match response.status with
    | `OK -> read_response response_body notify_response_received t.body
    | err -> failwith @@ H2.Status.to_string err
;;

let make_trailers_handler t headers = t.trailers <- headers

let call conn uri ?(headers = Grpc_basic.Headers.empty) body =
  let scheme, host, port, path = parse_uri uri in
  let t = init () in
  let error_handler = make_error_handler t in
  let headers =
    headers
    |> Grpc_basic.Headers.add_pseudo_headers
         [ "authority", Printf.sprintf "%s:%d" host port ]
  in
  let req = H2.Request.create ~scheme ~headers `POST path in
  let () =
    Lwt.async
    @@ fun () ->
    Logs_lwt.debug ~src:Log.src
    @@ fun m ->
    m
      ~header:Log.header
      ~tags:Log.(Tag.empty |> Tag.upstream_header headers |> Tag.uri uri)
      "send request"
  in
  let response_received, notify_response_received = Lwt.wait () in
  let response_handler = make_response_handler t notify_response_received in
  let trailers_handler = make_trailers_handler t in
  let request_body =
    H2_lwt_unix.Client.request conn req ~error_handler ~response_handler ~trailers_handler
  in
  let () = H2.Body.write_string request_body body in
  let () = H2.Body.close_writer request_body in
  let%lwt () = response_received in
  let () =
    Lwt.async
    @@ fun () ->
    Logs_lwt.debug ~src:Log.src
    @@ fun m ->
    m
      ~header:Log.header
      ~tags:
        Log.(
          Tag.empty
          |> Tag.downstream_header t.headers
          |> Tag.downstream_body @@ Buffer.contents t.body)
      "receive response"
  in
  Lwt.return t
;;
