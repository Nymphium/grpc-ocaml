open Import

open struct
  module M = struct
    module T = T.Byte_buffer
    module F = F.Byte_buffer
  end
end

let inspect = Inspect.make_structure (module M.T)

let allocate () =
  let finalise t = Lwt.async @@ fun () -> u @@ F.Byte_buffer.destroy @@ Ctypes.addr t in
  let t = Ctypes.make ~finalise T.Byte_buffer.t in
  t @. T.Byte_buffer.reserved <-@ __reserved__;
  Ctypes.addr t
;;

let from_string s =
  let*? slice = F.Slice.from_static_string s in
  let*? buf = F.Byte_buffer.create_raw (Ctypes.addr slice) (Unsigned.Size_t.of_int 1) in
  let*? () = F.Slice.unref slice in
  Lwt.return buf
;;

let to_string b =
  if Ctypes.is_null b
  then Lwt.return None
  else (
    let%lwt len = Unsigned.Size_t.to_int =~|< F.Byte_buffer.length b in
    let str = Buffer.create len in
    let next = Ctypes.make T.Slice.t in
    let reader = Ctypes.make T.Byte_buffer.Reader.t in
    let%lwt is_buf_reader_initialized =
      ( < ) 0 =~|< F.Byte_buffer.Reader.init (Ctypes.addr reader) b
    in
    if not is_buf_reader_initialized
    then failwith "Error initializing byte_buffer_reader"
    else (
      let rec go () =
        let%lwt cond =
          F.Byte_buffer.Reader.next (Ctypes.addr reader) (Ctypes.addr next) >|~= ( = ) 0
        in
        if cond
        then Lwt.return ()
        else (
          let%lwt s = Slice.to_ocaml_string next in
          Buffer.add_string str s;
          F.Slice.unref next >>~= go)
      in
      let%lwt () = go () in
      let*? () = F.Byte_buffer.Reader.destroy (Ctypes.addr reader) in
      Lwt.return @@ Some (Buffer.contents str)))
;;
