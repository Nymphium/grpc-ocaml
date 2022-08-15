open Import

open struct
  module M = T.Channel
end

type t =
  { mutable channel : M.t
  ; credentials : T.Channel.Credentials.t
  }

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
  type t = M.Credentials.t

  let free crd = F.Channel.Credentials.release crd

  let make_insecure () =
    let () = Top.init () in
    F.Channel.Credentials.create_insecure ()
  ;;
end

let free_internal = F.Channel.destroy
let get_target t = F.Channel.get_target t.channel

let make ?(credentials = Credentials.make_insecure ()) target args =
  Top.init ();
  let args = if List.length args > 0 then Some (addr @@ Args.make args) else None in
  let channel = F.Channel.create target credentials args in
  let () = F.Channel.Credentials.release credentials in
  { credentials; channel }
;;

let get_connectivity_state ?(try_to_connect = false) t =
  let try_to_connect = if try_to_connect then 1 else 0 in
  F.Channel.Connectivity.State.check t.channel try_to_connect
;;
