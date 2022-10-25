module Reader = struct
  let unary body =
    let io, notifier = Lwt.wait () in
    let buf = Buffer.create 128 in
    let rec go counter =
      H2.Body.Reader.schedule_read
        body
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
          Buffer.add_bytes buf response_fragment;
          go (counter + 1))
        ~on_eof:(fun () ->
          Fun.protect ~finally:(fun () -> Buffer.reset buf)
          @@ fun () -> Lwt.wakeup_later notifier @@ Buffer.contents buf)
    in
    go 0;
    io
  ;;
end

let default_headers = Grpc_basic.Headers.(empty |> Content_type.set_grpc_proto)

let catch th =
  Lwt.catch th (fun exn ->
      Lwt.return
      @@ Base.make_response_trailers (Error (`INTERNAL, Some (Printexc.to_string exn))))
;;

let unary handler context reqd =
  let request_body = H2.Reqd.request_body reqd in
  let res = H2.Response.create `OK ~headers:default_headers in
  let writer = H2.Reqd.respond_with_streaming reqd res in
  let open Lwt.Infix in
  let open Lwt.Syntax in
  Lwt.async
  @@ fun () ->
  let* body = Reader.unary request_body in
  let* compressed, body = Base.decode_body_frame body in
  (* TODO: handle compressed body *)
  assert (not compressed);
  let+ msg, trailers =
    catch (fun () -> handler context body >|= Base.make_response_trailers)
  in
  let () =
    Lwt.async
    @@ fun () ->
    Logs_lwt.debug ~src:Log.src
    @@ fun m ->
    m
      "send response"
      ~header:Log.header
      ~tags:
        Log.(
          Tag.empty
          |> Tag.downstream_body msg
          |> Tag.downstream_header res.H2.Response.headers)
  in
  H2.Reqd.schedule_trailers reqd trailers;
  H2.Body.Writer.write_string writer msg;
  H2.Body.Writer.close writer
;;
