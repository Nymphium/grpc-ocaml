(** length of the representation of compress-flag *)
let compress_flag_length = 1

(** length of string which represents the length of messages *)
let message_length_length = 4

let frame_offset = compress_flag_length + message_length_length

(** compress_flag_length -length representation of uncompressed *)
let uncompressed_flag = "\000"

let uncompressed_flag_c =
  assert (compress_flag_length = 1);
  uncompressed_flag.[0]
;;

(** compress_flag_length -length representation of uncompressed *)
let compressed_flag = "\001"

let compressed_flag_c =
  assert (compress_flag_length = 1);
  compressed_flag.[0]
;;
