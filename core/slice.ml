open Import

open struct
  module M = T.Slice
end

let allocate ?(size = 0) () =
  let size = Unsigned.Size_t.of_int size in
  F.Slice.malloc size
;;

let start_ptr = F.Slice.start_ptr
let length = Unsigned.Size_t.to_int <@ F.Slice.length

let set_length slice newlen =
  let newlen = Unsigned.Size_t.of_int newlen in
  F.Slice.set_length slice newlen
;;

let end_ptr = F.Slice.end_ptr
let is_empty = ( < ) 0 <@ F.Slice.is_empty
let to_string = F.Slice.to_c_string

let raw_bytes slice =
  let start = start_ptr slice in
  if is_null @@ start
  then Lwt.fail_with "attempt to uninitialized slice to ocaml string"
  else (
    let length = length slice in
    let arr = CArray.from_ptr start length in
    CArray.to_list arr
    |> List.map Unsigned.UInt8.to_hexstring
    |> String.concat " "
    |> Printf.sprintf "< %s >"
    |> Lwt.return)
;;

let from_static_string = F.Slice.from_static_string

let from_copied_buffer s l =
  let l = Unsigned.Size_t.of_int l in
  F.Slice.from_copied_buffer s l
;;

let unref = F.Slice.unref
let inspect = Inspect.make_structure (module M)

let%test "slice iso" =
  let slice = from_static_string "hello" in
  let _ = to_string slice in
  let _ = to_string slice in
  let s' = to_string slice in
  let () = unref slice in
  String.equal s' "hello"
;;

let%test "is empty" =
  let slice = from_static_string "hello" in
  let is_empty = is_empty slice in
  let () = unref slice in
  not is_empty
;;

let%test "slice allocation identity" =
  let b1 = allocate () in
  let b2 = allocate () in
  let res = not (b1 == b2) in
  let () = unref b1 in
  let () = unref b2 in
  res
;;
