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
let global_connection_polling_mu = from_voidp T.Sync.Mu.t null
let global_connection_polling_cv = from_voidp T.Sync.Cv.t null
let channel_polling_cq = from_voidp T.Completion.Queue.t null

let get_bg { bg; _ } =
  match bg with
  | None -> failwith "closed!"
  | Some v -> v
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
  let free crd = F.Channel.Credentials.release crd

  let make_insecure () =
    let () = Top.init () in
    F.Channel.Credentials.create_insecure ()
  ;;
end

let free_internal = F.Channel.destroy

let init_try_register_connection_polling stack =
  let watched =
    let next = Lwt_mvar.take_available bg_watched_channel_list_head in
    { channel = stack.channel; channel_destroyed = false; next; refcount = 1 }
  in
  stack.wrapped.bg <- Some watched
;;

let get_target t =
  let bg = get_bg t in
  F.Channel.get_target bg.channel
;;

let make ?credentials target args =
  let credentials =
    Option.value ~default:(Credentials.make_insecure ()) credentials
    (* match credentials with *)
    (* | None -> Credentials.make_insecure () *)
    (* | Some c -> c *)
  in
  let args = if List.length args > 0 then Some (addr @@ Args.make args) else None in
  let c = F.Channel.create target credentials args in
  let () = F.Channel.Credentials.release credentials in
  let wrapped = { credentials; bg = None } in
  let stack = { channel = c; wrapped } in
  let () = init_try_register_connection_polling stack in
  wrapped
;;

let get_connectivity_state ?(try_to_connect = false) t =
  let try_to_connect = if try_to_connect then 1 else 0 in
  let bg =
    match t.bg with
    | None -> failwith "uninitialized channel"
    | Some v -> v
  in
  F.Channel.Connectivity.State.check bg.channel try_to_connect
;;
