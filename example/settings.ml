let host = "localhost"
let port = 50051

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter @@ Logs_fmt.reporter ();
  Logs.set_level (Some Logs.Debug)
;;
