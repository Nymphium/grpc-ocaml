open Core
open Grpc_core

open struct
  let genprefix = Printf.sprintf "%s%s"
  let grpc_header_prefix = genprefix "grpc-"
  let ct_prefix = genprefix "application/grpc"
end

type t = Metadata.bwd

let eq_key = String.equal
let pp = Metadata.pp_bwd
let show = Metadata.show_bwd
let empty : t = []
let append_list = ( @ )
let get k h : Metadata.value option = List.Assoc.find ~equal:eq_key h k
let[@inline] add k v h : t = List.Assoc.add ~equal:eq_key h k v
let[@inline] upsert k v h = List.Assoc.remove ~equal:eq_key h k |> add v k

(** manipulate `grpc-timeout` header
 see https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md *)
module Timeout = struct
  open struct
    let attr = grpc_header_prefix "timeout"
  end

  let get = get attr
  let set_second sec = add attr @@ Printf.sprintf "%dS" sec
  let upsert_second sec = upsert attr @@ Printf.sprintf "%dS" sec
end

module Content_type = struct
  open struct
    let attr = "content-type"
    let add = upsert attr
    let upsert = upsert attr
  end

  let get = get attr
  let set_grpc_proto = add @@ ct_prefix "+proto"
  let upsert_grpc_proto = upsert @@ ct_prefix "+proto"
  let set_grpc = add @@ ct_prefix ""
  let upsert_grpc = upsert @@ ct_prefix ""
end

module Trailers = struct
  open struct
    let message = grpc_header_prefix "message"
    let status = grpc_header_prefix "status"
  end

  let get_grpc_message = get message
  let set_grpc_message = add message
  let upsert_grpc_message = upsert message
  let get_grpc_status = get status
  let set_grpc_status = add status
  let upsert_grpc_status = upsert status
end
