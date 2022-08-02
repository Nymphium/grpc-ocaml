open Import

open struct
  module M = T.Timespec
  module CT = M.Clock_type
end

module Clock_type = struct
  open CT

  type t = CT.t

  let monotonic = MONOTONIC
  let realtime = REALTIME
  let timespan = TIMESPAN
  let precise = PRECISE
  let convert t ct = u @@ F.Timespec.convert_clock_type t ct
end

type t = M.t Ctypes.structure

let sec_of_int = Int64.of_int
let nsec_of_int = Int32.of_int
let int_of_sec = Int64.to_int
let int_of_nsec = Int32.to_int

let init =
  let it = lazy (u @@ F.Timespec.init ()) in
  fun () -> Lazy.force it
;;

let make ?(sec = 0) ?(nsec = 0) () =
  let%lwt () =
    if sec < 0 || nsec < 0 then Lwt.fail_with "time must be positive" else Lwt.return_unit
  in
  let sec = sec_of_int sec in
  let nsec = nsec_of_int nsec in
  let t = Ctypes.make M.t in
  let () =
    t @. M.tv_sec <-@ sec;
    t @. M.tv_nsec <-@ nsec;
    t @. M.clock_type <-@ Clock_type.realtime
  in
  Lwt.return t
;;

let zero ct = u @@ F.Timespec.zero ct
let now ct = u @@ F.Timespec.now ct
let inf_future ct = u @@ F.Timespec.inf_future ct
let inf_past ct = u @@ F.Timespec.inf_past ct
let cmp t t' = u @@ F.Timespec.cmp t t'
let max t t' = u @@ F.Timespec.max t t'
let min t t' = u @@ F.Timespec.min t t'
let add t t' = u @@ F.Timespec.add t t'
let sub t t' = u @@ F.Timespec.sub t t'
let from_micros i ct = u @@ F.Timespec.from_micros i ct
let from_nanos i ct = u @@ F.Timespec.from_nanos i ct
let from_millis i ct = u @@ F.Timespec.from_millis i ct
let from_seconds i ct = u @@ F.Timespec.from_seconds i ct
let from_minutes i ct = u @@ F.Timespec.from_minutes i ct
let from_hours i ct = u @@ F.Timespec.from_hours i ct
let until t = u @@ F.Timespec.until t
let to_micros t = u @@ F.Timespec.to_micros t
let to_millis t = u @@ F.Timespec.to_millis t
