open Import
include T.Byte_buffer
include F.Byte_buffer

module Reader = struct
  include T.Byte_buffer.Reader
  include F.Byte_buffer.Reader

  type raw = t

  let raw = t
end

type raw = t

let raw = t

let allocate () =
  let finalise t = destroy @@ Ctypes.addr t in
  let t = Ctypes.make ~finalise raw in
  t @. reserved <-@ __reserved__;
  Ctypes.addr t
;;

let length = Unsigned.Size_t.to_int <@ length

let from_string ?copy s =
  let slice = Slice.from_string ?copy s in
  let buf = create_raw (Ctypes.addr slice) (Unsigned.Size_t.of_int 1) in
  let () = Slice.unref slice in
  buf
;;

let to_string_opt b =
  if Ctypes.is_null b
  then None
  else (
    let reader = malloc Reader.t in
    let is_buf_reader_initialized = 0 < Reader.init reader b in
    if not is_buf_reader_initialized
    then failwith "Error initializing byte_buffer_reader"
    else (
      let b = Reader.readall reader in
      Some (Slice.to_string b)))
;;
