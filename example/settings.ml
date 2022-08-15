let host = "localhost"
let port = 20000

let () =
  ignore
  @@ Lwt_main.Enter_iter_hooks.add_first
  @@ fun () ->
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter @@ Logs_fmt.reporter ();
  Logs.set_level (Some Logs.Debug)
;;
