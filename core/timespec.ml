open Import

open struct
  module M = T.Timespec
  module CT = M.Clock_type
end

include F.Timespec

module Clock_type = struct
  open CT

  type t = CT.t

  let monotonic = MONOTONIC
  let realtime = REALTIME
  let timespan = TIMESPAN
  let precise = PRECISE
  let convert = convert_clock_type
end

type t = M.t Ctypes.structure

type t' =
  [ `Millis of int64
  | `Seconds of int64
  ]

let sec_of_int = Int64.of_int
let nsec_of_int = Int32.of_int
let int_of_sec = Int64.to_int
let int_of_nsec = Int32.to_int

let make ?(sec = 0) ?(nsec = 0) () =
  let () = if sec < 0 || nsec < 0 then failwith "time must be positive" in
  let sec = sec_of_int sec in
  let nsec = nsec_of_int nsec in
  let t = Ctypes.make M.t in
  let () =
    t @. M.tv_sec <-@ sec;
    t @. M.tv_nsec <-@ nsec;
    t @. M.clock_type <-@ Clock_type.realtime
  in
  t
;;

let inf_future' = inf_future Clock_type.realtime
let inf_past' = inf_past Clock_type.realtime

let now_after (t : t') =
  let now = now Clock_type.realtime in
  let tic =
    match t with
    | `Millis millis -> from_millis millis Clock_type.timespan
    | `Seconds sec -> from_millis sec Clock_type.timespan
  in
  add now tic
;;
