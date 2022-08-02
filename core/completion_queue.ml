open Import

open struct
  module M = T.Completion.Queue
end

let pluck cq timeout tag =
  (* 1 tic 20msec *)
  let%lwt increment = Timespec.(from_millis 1000L Clock_type.timespan) in
  let add () =
    let%lwt now = Timespec.(now Clock_type.realtime) in
    Timespec.add now increment
  in
  let rec go () =
    let%lwt deadline = add () in
    let*? ev = F.Completion_queue.pluck cq tag deadline __reserved__ in
    let is_not_timeout = ev @.* T.Event.typ <> T.Completion.Type.QUEUE_TIMEOUT in
    let%lwt is_deadline = Timespec.cmp deadline timeout >|= ( < ) 0 in
    if is_not_timeout || is_deadline then Lwt.return ev else go ()
  in
  go ()
;;
