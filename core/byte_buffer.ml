open Import

open struct
  module M = struct
    module T = T.Byte_buffer
    module F = F.Byte_buffer
  end
end

let inspect = Inspect.make_structure (module M.T)

let allocate () =
  let finalise t = F.Byte_buffer.destroy @@ Ctypes.addr t in
  let t = Ctypes.make ~finalise T.Byte_buffer.t in
  t @. T.Byte_buffer.reserved <-@ __reserved__;
  Ctypes.addr t
;;

let length b = Unsigned.Size_t.to_int @@ F.Byte_buffer.length b
let destroy = F.Byte_buffer.destroy

let from_string s =
  let slice = F.Slice.from_static_string s in
  let buf = F.Byte_buffer.create_raw (Ctypes.addr slice) (Unsigned.Size_t.of_int 1) in
  let () = F.Slice.unref slice in
  buf
;;

let to_string_opt b =
  if Ctypes.is_null b
  then None
  else (
    let reader = malloc T.Byte_buffer.Reader.t in
    let is_buf_reader_initialized = 0 < F.Byte_buffer.Reader.init reader b in
    if not is_buf_reader_initialized
    then failwith "Error initializing byte_buffer_reader"
    else (
      let b = F.Byte_buffer.Reader.readall reader in
      Some (Slice.to_string b)))
;;

let%test "bb iso" =
  let buf = from_string "foobar" in
  let _ = to_string_opt buf in
  let _ = to_string_opt buf in
  let s' = to_string_opt buf in
  let () = destroy buf in
  s' = Some "foobar"
;;

let%test "length" =
  let buf = from_string "blength" in
  let len = length buf in
  let () = destroy buf in
  len = 7
;;

let%test "bb allocation identity" =
  let b1 = allocate () in
  let b2 = allocate () in
  let res = not (b1 == b2) in
  let () = destroy b1 in
  let () = destroy b2 in
  res
;;
