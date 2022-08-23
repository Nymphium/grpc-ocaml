open Import
include T.Metadata
include Array

type elem = T.Metadata.t

let elem = T.Metadata.t

type raw = t

let raw = t

(** assoc list as key-value metadata array *)
type bwd = (key * value) list [@@deriving show]

and key = string
and value = string

let init = F.Metadata.init
let destroy = F.Metadata.destroy

let destroy_entries md len =
  let md = CArray.from_ptr md len in
  flip CArray.iter md
  @@ fun elem ->
  Slice.unref (elem @.* key);
  Slice.unref (elem @.* value)
;;

(** allocate and initialize object with capacity = size *)
let allocate ?(size = 0) ?(count = 0) () =
  let t = malloc Array.t in
  let () = init t in
  let size = Unsigned.Size_t.of_int size in
  t |-> Array.capacity <-@ size;
  t |-> Array.count <-@ Unsigned.Size_t.of_int count;
  t
;;

open struct
  let fwd1 ~k ~v =
    let k' = Slice.from_string k in
    let key_is_valid = 0 < F.Header.key_is_legal k' in
    let () = if not key_is_valid then failwith @@ Printf.sprintf "invalid key: %s" k in
    let v' = Slice.from_string v in
    let value_is_valid = 0 < F.Header.nonbin_value_is_legal v' in
    let () =
      if not value_is_valid then failwith @@ Printf.sprintf "invalid value: %s" v
    in
    let vl = Ctypes.make elem in
    let () =
      vl @. key <-@ k';
      vl @. value <-@ v'
    in
    vl
  ;;

  let bwd1 md =
    let k = Slice.to_string @@ md @.* key in
    let v = Slice.to_string @@ md @.* value in
    k, v
  ;;
end

let make bwd =
  let len = List.length bwd in
  let arr = allocate ~size:len ~count:len () in
  bwd
  |> List.map (fun (k, v) -> fwd1 ~k ~v)
  |> fun l ->
  let md = CArray.(of_list elem l |> start) in
  arr |-> metadata <-@ md;
  arr
;;

let to_bwd fwd =
  if is_null fwd
  then []
  else (
    let size = Unsigned.Size_t.to_int (fwd |->* count) in
    let arr' = CArray.(to_list @@ flip from_ptr size (fwd |->* metadata)) in
    List.map bwd1 arr')
;;

let get_bwd t k = List.assoc_opt k t
