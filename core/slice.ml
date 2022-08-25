open Import
include T.Slice
include F.Slice

type raw = t

let raw = t

let[@inline] allocate ?(size = 0) () =
  let size = Unsigned.Size_t.of_int size in
  malloc size
;;

let length = Unsigned.Size_t.to_int <@ length

let[@inline] set_length slice newlen =
  let newlen = Unsigned.Size_t.of_int newlen in
  set_length slice newlen
;;

let is_empty = ( < ) 0 <@ is_empty
let to_string = to_c_string

let[@inline] from_copied_buffer s =
  let l = String.length s |> Unsigned.Size_t.of_int in
  from_copied_buffer s l
;;

let[@inline] from_string ?(copy = false) =
  if copy then from_copied_buffer else from_static_string
;;
