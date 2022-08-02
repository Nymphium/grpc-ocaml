open Import

open struct
  module M = T.Metadata
end

(** assoc list as key-value metadata array *)
type fwd = (string * string) list

let init arr = u @@ F.Metadata.init (Ctypes.addr arr)

(** allocate object with capacity = size *)
let allocate ?(size = 0) () =
  let finalise a = Lwt.async @@ fun () -> u @@ F.Metadata.destroy (Ctypes.addr a) in
  let t = Ctypes.make ~finalise T.Metadata.Array.t in
  let%lwt () = init t in
  let size = Unsigned.Size_t.of_int size in
  t @. M.Array.capacity <-@ size;
  t @. M.Array.count <-@ Unsigned.Size_t.zero;
  Lwt.return t
;;

let make fwd =
  let len = List.length fwd in
  let%lwt arr = allocate ~size:len () in
  fwd
  |> List.map (fun (k, v) ->
         let%lwt k' = Slice.from_static_string k in
         let%lwt valid = ( < ) 0 =~|< F.Header.key_is_legal k' in
         let%lwt () =
           if not valid
           then Lwt.fail_with @@ Printf.sprintf "invalid key: %s" k
           else Lwt.return_unit
         in
         let%lwt v = Slice.from_static_string v in
         let vl = Ctypes.make M.t in
         let () =
           vl @. M.key <-@ k';
           vl @. M.value <-@ v
         in
         Lwt.return vl)
  |> Lwt.all
  |> Lwt.map
     @@ fun l ->
     let md = CArray.(of_list M.t l |> start) in
     arr @. M.Array.metadata <-@ md;
     arr
;;

let to_fwd arr =
  let size = Unsigned.Size_t.to_int !@(arr |-> T.Metadata.Array.count) in
  let arr' =
    CArray.(to_list @@ flip from_ptr size !@(arr |-> T.Metadata.Array.metadata))
  in
  Lwt.all
  @@ flip List.map arr'
  @@ fun md ->
  let%lwt k = Slice.to_ocaml_string @@ md @.* T.Metadata.key in
  let%lwt v = Slice.to_ocaml_string @@ md @.* T.Metadata.value in
  Lwt.return (k, v)
;;

let inspect' t =
  let%lwt k = Slice.raw_bytes @@ t @.* M.key in
  let%lwt v = Slice.raw_bytes @@ t @.* M.value in
  Lwt.return @@ Printf.sprintf {|{ key: "%s"; value: "%s" }|} k v
;;
