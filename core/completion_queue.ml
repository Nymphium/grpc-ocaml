open Import

open struct
  module M = T.Completion.Queue

  let pluck =
    let f =
      Lwt_preemptive.detach (fun (cq, tag, deadline) ->
          F.Completion_queue.pluck cq tag deadline __reserved__)
    in
    fun cq tag deadline -> f (cq, tag, deadline)
  ;;
end

(** 1 tic 20 msec by default *)
let tic : [ `Millis of int64 | `Seconds of int64 ] ref = ref (`Millis 20L)

let create_for_pluck () = F.Completion_queue.create_for_pluck __reserved__
let destroy = F.Completion_queue.destroy

let pluck cq timeout tag =
  let next_tic () = Timespec.now_after !tic in
  let rec go () =
    let deadline = next_tic () in
    let%lwt ev = pluck cq tag deadline in
    let is_not_timeout = Event.type_of ev <> T.Completion.Type.QUEUE_TIMEOUT in
    let is_deadline = Timespec.cmp deadline timeout > 0 in
    if is_not_timeout || is_deadline then Lwt.return ev else go ()
  in
  go ()
;;
