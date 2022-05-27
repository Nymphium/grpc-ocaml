open Core

module Hex : sig
  type encoded = bytes
  type decoded = int

  val unsafe_to_string : encoded -> string
  val unsafe_of_string : string -> encoded
  val concat : encoded list -> encoded
  val encode : ?min_digit:int -> decoded -> encoded
  val decode : encoded -> decoded
end = struct
  (** representation (e.g. "\222\173\190\239") *)
  type encoded = bytes

  (** value  (e.g. 0xdeadbeef) *)
  type decoded = int

  external unsafe_to_string : encoded -> string = "%bytes_to_string"
  external unsafe_of_string : string -> encoded = "%bytes_of_string"

  let concat = Stdlib.Bytes.(concat @@ create 0)

  let encode =
    let rec go digit acc rest =
      let x = Int.pow 0x100 digit in
      let rest, bit' = rest / 0x100, rest mod x in
      let acc = Char.unsafe_of_int bit' :: acc in
      if rest = 0 then digit, acc else go (digit + 1) acc rest
    in
    fun ?(min_digit = 0) d ->
      let size, ls = go 1 [] d in
      let byts = Bytes.of_char_list ls in
      let diff = min_digit - size in
      if diff > 0
      then (
        let res = Bytes.make min_digit '\000' in
        let () = Bytes.blit ~src:byts ~src_pos:0 ~dst:res ~dst_pos:diff ~len:size in
        res)
      else byts
  ;;

  let%test_unit _ =
    [%test_eq: Bytes.t] (encode ~min_digit:2 0x3) (Bytes.of_string "\000\003")
  ;;

  let%test_unit _ =
    [%test_eq: Bytes.t] (encode ~min_digit:2 0xffffff) (Bytes.of_string "\255\255\255")
  ;;

  let%test_unit _ = [%test_eq: Bytes.t] (encode 0x2f4f3) (Bytes.of_string "\002\244\243")

  let decode e =
    Bytes.to_list e
    |> List.rev
    |> List.foldi ~init:0 ~f:(fun digit acc c ->
           let x = Int.pow 0x100 digit in
           acc + (Char.to_int c * x))
  ;;

  let%test_unit _ = [%test_eq: Int.t] (decode (Bytes.of_string "\003")) 0x3

  let%test_unit _ =
    [%test_eq: Int.t] (decode (Bytes.of_string "\000\255\255\255")) 0xffffff
  ;;

  let%test_unit _ = [%test_eq: Int.t] (decode (encode 0x3)) 0x3
  let%test_unit _ = [%test_eq: Int.t] (decode (encode 0xffffff)) 0xffffff
end
