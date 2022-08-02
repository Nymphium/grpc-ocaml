open Import

open struct
  module M = T.Metadata
end

(** assoc list as key-value metadata array *)
type fwd = (string * string) list

let make fwd =
  let len = List.length fwd in
  let arr = Ctypes.make M.Array.t in
  let () =
    arr @. M.Array.capacity <-@ Unsigned.Size_t.of_int len;
    arr @. M.Array.count <-@ Unsigned.Size_t.of_int len
  in
  let*? () = F.Metadata.init @@ Ctypes.addr arr in
  fwd
  |> List.map (fun (k, v) ->
         let%lwt k = Slice.from_static_string k in
         let%lwt v = Slice.from_static_string v in
         let vl = Ctypes.make M.t in
         let%lwt () = Lwt_io.printl ">>>" in
         let%lwt () = Lwt_io.printl =<< Slice.raw_bytes k in
         let%lwt () = Lwt_io.printl =<< Slice.raw_bytes v in
         let%lwt () = Lwt_io.printl "aaa" in
         let () =
           vl @. M.key <-@ k;
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

let inspect' t =
  let%lwt k = Slice.raw_bytes @@ deref @@ t @. M.key in
  let%lwt v = Slice.raw_bytes @@ deref @@ t @. M.value in
  Lwt.return @@ Printf.sprintf {|{ key: "%s"; value: "%s" }|} k v
;;
(* let buf = Buffer.create 512 in *)
(* let buf_fmt = Format.formatter_of_buffer buf in *)
(* Ctypes.format M.t buf_fmt t; *)
(* Buffer.contents buf *)
