open Import

open struct
  module M = T.Slice
end

let allocate ?(size = 0) () =
  let size = Unsigned.Size_t.of_int size in
  u @@ F.Slice.malloc size
;;

let start_ptr = u <@ F.Slice.start_ptr
let length = Unsigned.Size_t.to_int <~@ F.Slice.length

let set_length slice newlen =
  let newlen = Unsigned.Size_t.of_int newlen in
  u @@ F.Slice.set_length slice newlen
;;

let end_ptr = u <@ F.Slice.end_ptr
let is_empty = ( >= ) 0 <~@ F.Slice.is_empty

let to_ocaml_string slice =
  let%lwt start = start_ptr slice in
  if is_null start
  then Lwt.fail_with "attempt to uninitialized slice to ocaml string"
  else Lwt.return @@ coerce (ptr uint8_t) string start
;;

let raw_bytes slice =
  let%lwt start = start_ptr slice in
  if is_null @@ start
  then Lwt.fail_with "attempt to uninitialized slice to ocaml string"
  else (
    let%lwt length = length slice in
    let arr = CArray.from_ptr start length in
    CArray.to_list arr
    |> List.map Unsigned.UInt8.to_hexstring
    |> String.concat " "
    |> Printf.sprintf "< %s >"
    |> Lwt.return)
;;

let from_static_string = u <@ F.Slice.from_static_string
let from_copied_string = u <@ F.Slice.from_copied_string
let unref = u <@ F.Slice.unref
let inspect = Inspect.make_structure (module M)
