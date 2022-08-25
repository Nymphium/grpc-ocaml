open Import

open struct
  let is_initialized = F.is_initialized
  let sw_shutdown = Lwt_switch.create ()
  let init = F.init
  let shutdown = F.shutdown
  let () = Lwt_switch.add_hook (Some sw_shutdown) @@ Lwt.wrap1 shutdown
end

let core_version = F.version_s ()

let init () =
  if is_initialized () <> 1
  then (
    Log.message
      __FILE__
      __LINE__
      `Debug
      (Printf.sprintf "grpc-ocaml with grpc-core %s(%s)" core_version (F.g_stands_for ()));
    init ())
;;

let _ = Lwt_main.Exit_hooks.add_last @@ fun () -> Lwt_switch.turn_off sw_shutdown
