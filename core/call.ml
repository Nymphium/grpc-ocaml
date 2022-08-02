open Import

open struct
  module M = T.Call
end

type hoge_values

let hoge : hoge_values structure typ = Ctypes.structure "hoge_values"
let hoge_f1 = Ctypes.field hoge "f1" Ctypes.string
let () = Ctypes.seal hoge

(** wrapped call data *)
type t =
  { call : M.t
  ; cq : T.Completion.Queue.t
  ; flags : T.Flags.Write.t
  }

let run_batch t ops =
  let size_ops = List.length ops |> Unsigned.Size_t.of_int in
  let tag = Ctypes.null in
  let%lwt ops =
    CArray.(start <@ of_list T.Op.t)
    <@ List.map (fun op ->
           op @. T.Op.flags <-@ t.flags;
           op)
    =|< Op.make_ops ops
  in
  (* Ctypes.format T.Op.t Format.std_formatter !@ops; *)
  (* flush stdout; *)
  let*? err = F.Call.start_batch t.call ops size_ops tag __reserved__ in
  let%lwt () =
    if err <> M.Error.OK
    then Lwt.fail_with @@ Printf.sprintf "prepare call error(%s)" @@ M.Error.show err
    else Lwt.return_unit
  in
  let%lwt inf = Timespec.(inf_future Clock_type.realtime) in
  let%lwt ev = Completion_queue.pluck t.cq inf tag in
  if deref @@ ev @. T.Event.success < 1
  then (
    let st = deref @@ ev @. T.Event.typ in
    Lwt.fail_with @@ Printf.sprintf "run_batch failed(%s)" @@ T.Completion.Type.show st)
  else Lwt.return (tag, ops)
;;
