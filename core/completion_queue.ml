open Import

open struct
  module M = T.Completion.Queue
end

(** 1 tic 20 msec by default *)
let tic : Timespec.t' ref = ref (`Millis 20L)

let create_for_pluck () = F.Completion_queue.create_for_pluck __reserved__

let destroy t =
  F.Completion_queue.shutdown t;
  F.Completion_queue.destroy t
;;

let pluck cq timeout tag =
  let next_tic () = Timespec.now_after !tic in
  let rec go () =
    let deadline = next_tic () in
    let ev = F.Completion_queue.pluck cq tag deadline __reserved__ in
    let is_not_timeout = Event.type_of ev <> T.Completion.Type.QUEUE_TIMEOUT in
    let is_deadline = Timespec.cmp deadline timeout > 0 in
    if is_not_timeout || is_deadline then ev else go ()
  in
  go ()
;;
