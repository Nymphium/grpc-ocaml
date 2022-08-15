open Import

open struct
  module M = T.Log
end

type log_level =
  [ `Debug
  | `Error
  | `Info
  ]

let log_level_of_severity = function
  | `Debug -> M.Severity.DEBUG
  | `Error -> M.Severity.ERROR
  | `Info -> M.Severity.INFO
;;

let severity_of_log_level = function
  | M.Severity.DEBUG -> `Debug
  | ERROR -> `Error
  | INFO -> `Info
;;

let init =
  let it = lazy (F.Log.init ()) in
  fun () -> Lazy.force it
;;

let should_log log_level =
  let severity = log_level_of_severity log_level in
  F.Log.should_log severity > 0
;;

let set_log_level log_level =
  let severity = log_level_of_severity log_level in
  F.Log.set_log_level severity
;;

let string_of_log_level = function
  | `Debug -> "debug"
  | `Error -> "error"
  | `Info -> "info"
;;

let set_log_callback fn =
  let fn args =
    let args = !@args in
    let<* file = args @. M.Func_args.file in
    let<* line = args @. M.Func_args.line in
    let log_level = args @. M.Func_args.severity |@.> severity_of_log_level in
    let<* message = args @. M.Func_args.message in
    fn ~file ~line ~log_level ~message
  in
  F.Log.set_log_func fn
;;

let message file line severity msg =
  let f file line severity msg =
    let mf = Lwt_preemptive.detach F.Log.message in
    Lwt.async @@ fun () -> mf file >|= fun f' -> f' line severity msg
  in
  let severity = log_level_of_severity severity in
  f file line severity msg
;;
