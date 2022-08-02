open Import

open struct
  module M = T.Channel
end

type bg_watched_channel =
  { mutable channel : M.t
  ; mutable next : bg_watched_channel option
  ; mutable channel_destroyed : bool
  ; mutable refcount : int
  }

type t =
  { mutable bg : bg_watched_channel option
  ; credentials : T.Channel.Credentials.t
  }

type init_try_register_stack =
  { mutable channel : M.t
  ; mutable wrapped : t
  }

let bg_watched_channel_list_head = Lwt_mvar.create_empty ()
let global_connection_polling_mu = coerce (ptr void) T.Sync.Mu.t null
let global_connection_polling_cv = coerce (ptr void) T.Sync.Cv.t null
let channel_polling_cq = coerce (ptr void) T.Completion.Queue.t null

let get_bg { bg; _ } =
  match bg with
  | None -> Lwt.fail_with "closed!"
  | Some v -> Lwt.return v
;;

module Args = struct
  let inspect = Inspect.make (module M)

  let make (assoc : (string * Args.Value.v) list) =
    let size = List.length assoc in
    let t = Ctypes.make M.Arg.t in
    let args = CArray.of_list T.Arg.t @@ List.map (fun (k, v) -> Args.make k v) assoc in
    let () =
      t @. M.Arg.num_args <-@ Unsigned.Size_t.of_int size;
      t @. M.Arg.args <-@ CArray.start args
    in
    t
  ;;
end

module Credentials = struct
  let free crd =
    let*? () = F.Channel.Credentials.release crd in
    Top.shutdown ()
  ;;

  let make_insecure () =
    let%lwt () = Top.init () in
    u @@ F.Channel.Credentials.create_insecure ()
  ;;
end

let free_internal ch = u @@ F.Channel.destroy ch

let init_try_register_connection_polling stack =
  let watched =
    let next = Lwt_mvar.take_available bg_watched_channel_list_head in
    { channel = stack.channel; channel_destroyed = false; next; refcount = 1 }
  in
  stack.wrapped.bg <- Some watched
;;

let get_target t =
  let%lwt bg = get_bg t in
  u @@ F.Channel.get_target bg.channel
;;

let make ?credentials target args =
  let%lwt credentials =
    match credentials with
    | None -> Credentials.make_insecure ()
    | Some c -> Lwt.return c
  in
  let args = if List.length args > 0 then Some (addr @@ Args.make args) else None in
  let*? c = F.Channel.create target credentials args in
  let*? () = F.Channel.Credentials.release credentials in
  let wrapped = { credentials; bg = None } in
  let stack = { channel = c; wrapped } in
  let () = init_try_register_connection_polling stack in
  Lwt.return wrapped
;;

let get_connectivity_state ?(try_to_connect = false) t =
  let try_to_connect = if try_to_connect then 1 else 0 in
  let%lwt bg =
    match t.bg with
    | None -> Lwt.fail_with "uninitialized channel"
    | Some v -> Lwt.return v
  in
  u @@ F.Channel.Connectivity.State.check bg.channel try_to_connect
;;

let make_call ~channel:t ?parent ?(flags = 0) ~methd ?host ?deadline () =
  let%lwt host =
    match host with
    | Some host -> Lwt.return host
    | None -> get_target t
  in
  let flags = Unsigned.UInt32.of_int flags in
  let%lwt host_slice = Slice.from_static_string host in
  let parent_call =
    match parent with
    | None -> coerce (ptr void) T.Call.t null
    | Some parent -> parent.Call.call
  in
  let*? cq = F.Completion_queue.create_for_pluck null in
  let%lwt methd_slice = Slice.from_static_string methd in
  let%lwt bg = get_bg t in
  let%lwt deadline =
    match deadline with
    | Some deadline -> Lwt.return deadline
    | None -> Timespec.(inf_future Clock_type.realtime)
  in
  let*? call =
    F.Channel.create_call
      bg.channel
      parent_call
      flags
      cq
      methd_slice
      (Ctypes.addr host_slice)
      deadline
      null
  in
  let%lwt () = Slice.unref methd_slice in
  let%lwt () = Slice.unref host_slice in
  Lwt.return Call.{ cq; call; flags }
;;
