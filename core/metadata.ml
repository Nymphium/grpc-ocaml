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
  let msize = Unsigned.Size_t.of_int (size * sizeof elem) in
  let size = Unsigned.Size_t.of_int size in
  t |-> Array.metadata <-@ from_voidp elem @@ F.Alloc.zalloc msize;
  t |-> Array.capacity <-@ size;
  t |-> Array.count <-@ Unsigned.Size_t.of_int count;
  t
;;

open struct
  let fwd1 (k, v) elem =
    let k' = Slice.from_string ~copy:true k in
    let key_is_valid = F.Header.key_is_legal k' > 0 in
    let () =
      if not key_is_valid
      then (
        Slice.unref k';
        failwith @@ Printf.sprintf "invalid key: %s" k)
    in
    let v' = Slice.from_string ~copy:true v in
    let value_is_valid = F.Header.nonbin_value_is_legal v' > 0 in
    let () =
      if not value_is_valid
      then (
        Slice.unref v';
        failwith @@ Printf.sprintf "invalid value: %s" v)
    in
    elem @. key <-@ k';
    elem @. value <-@ v'
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
  let count = ref 0 in
  CArray.from_ptr (arr |->* metadata) len
  |> CArray.iter (fun elem ->
         let kv = List.nth bwd !count in
         fwd1 kv elem;
         incr count);
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
