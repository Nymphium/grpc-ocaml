open Import
include T.Slice
include F.Slice

type raw = t

let raw = t

let with_finalise s =
  Gc.finalise unref s;
  s
;;

let allocate ?(size = 0) () =
  let size = Unsigned.Size_t.of_int size in
  let s = malloc size in
  with_finalise s
;;

let length = Unsigned.Size_t.to_int <@ length

let set_length slice newlen =
  let newlen = Unsigned.Size_t.of_int newlen in
  set_length slice newlen
;;

let is_empty = ( < ) 0 <@ is_empty
let to_string = to_c_string
let from_static_string = with_finalise <@ from_static_string

let from_copied_buffer s =
  with_finalise
  @@
  let l = String.length s |> Unsigned.Size_t.of_int in
  from_copied_buffer s l
;;

let from_string ?(copy = false) = if copy then from_copied_buffer else from_static_string
