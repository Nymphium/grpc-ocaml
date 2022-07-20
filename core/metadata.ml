open Import

open struct
  module M = T.Metadata
end

(** assoc list as key-value metadata array *)
type bwd = (key * value) list [@@deriving show]

and key = string
and value = string

let init = F.Metadata.init

(** allocate and initialize object with capacity = size *)
let allocate ?(size = 0) ?(count = 0) () =
  let t = malloc T.Metadata.Array.t in
  let () = init t in
  let size = Unsigned.Size_t.of_int size in
  t |-> M.Array.capacity <-@ size;
  t |-> M.Array.count <-@ Unsigned.Size_t.of_int count;
  t
;;

let destroy = F.Metadata.destroy

open struct
  let fwd1 ~k ~v =
    let k' = Slice.from_static_string k in
    let valid = 0 < F.Header.key_is_legal k' in
    let () = if not valid then failwith @@ Printf.sprintf "invalid key: %s" k in
    let v = Slice.from_static_string v in
    let vl = Ctypes.make M.t in
    let () =
      vl @. M.key <-@ k';
      vl @. M.value <-@ v
    in
    vl
  ;;

  let bwd1 md =
    let k = Slice.to_string @@ md @.* T.Metadata.key in
    let v = Slice.to_string @@ md @.* T.Metadata.value in
    k, v
  ;;
end

let make bwd =
  let len = List.length bwd in
  let arr = allocate ~size:len ~count:len () in
  bwd
  |> List.map (fun (k, v) -> fwd1 ~k ~v)
  |> fun l ->
  let md = CArray.(of_list M.t l |> start) in
  arr |-> M.Array.metadata <-@ md;
  arr
;;

let to_bwd fwd =
  if is_null fwd
  then []
  else (
    let size = Unsigned.Size_t.to_int !@(fwd |-> T.Metadata.Array.count) in
    let arr' =
      CArray.(to_list @@ flip from_ptr size !@(fwd |-> T.Metadata.Array.metadata))
    in
    List.map bwd1 arr')
;;

let get_bwd t k = List.assoc_opt k t

let inspect' t =
  let%lwt k = Slice.raw_bytes @@ t @.* M.key in
  let%lwt v = Slice.raw_bytes @@ t @.* M.value in
  Lwt.return @@ Printf.sprintf {|{ key: "%s"; value: "%s" }|} k v
;;

let%test "md1 iso" =
  let k = "hello" in
  let v = "world" in
  let fwd1 = fwd1 ~k ~v in
  let k', v' = bwd1 fwd1 in
  k = k' && v = v'
;;

let%test "md iso" =
  let bwd = [ "hello", "world" ] in
  let fwd = make bwd in
  let bwd' = to_bwd fwd in
  let () = destroy fwd in
  bwd = bwd'
;;
