open Import

open struct
  let is_initialized = F.is_initialized
  let is_shutdowed = Lwt_mvar.create false
  let init = F.init
  let shutdown = F.shutdown
end

let core_version = F.version_s ()

let init () =
  if is_initialized () <> 1
  then
    Log.message
      __FILE__
      __LINE__
      `Debug
      (Printf.sprintf "grpc-ocaml with grpc-core %s(%s)" core_version (F.g_stands_for ()));
  init ()
;;

let shutdown () =
  let%lwt is_shutdowed' = Lwt_mvar.take is_shutdowed in
  if is_shutdowed'
  then Lwt.return_unit
  else (
    let%lwt () = Lwt_mvar.put is_shutdowed true in
    Lwt.return @@ shutdown ())
;;

let () = Lwt_main.Enter_iter_hooks.add_first init |> ignore
