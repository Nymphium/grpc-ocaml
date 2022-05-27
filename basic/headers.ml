open Core

open struct
  let genprefix = Printf.sprintf "%s%s"
  let grpc_header_prefix = genprefix "grpc-"
  let ct_prefix = genprefix "application/grpc"
end

type t = H2.Headers.t

let pp = H2.Headers.pp_hum

let show t =
  let () = pp Format.str_formatter t in
  Format.flush_str_formatter ()
;;

let of_list = H2.Headers.of_list
let to_list = H2.Headers.to_list
let empty = H2.Headers.empty
let append_list = H2.Headers.add_list
let get = Fn.flip H2.Headers.get
let[@inline] add ?sensitive k v h = H2.Headers.add ?sensitive h k v

let[@inline] add_pseudo_headers ph h =
  let ph' = List.rev_map ph ~f:(fun (k, v) -> String.concat [ ":"; k ], v) in
  H2.Headers.of_rev_list (H2.Headers.to_rev_list h @ ph')
;;

let%test_unit _ =
  let expected =
    show
    @@ of_list
         [ ":authority", "example.com"; ":path", "/hello"; "foo", "bar"; "qoo", "qux" ]
  in
  let actual =
    show
    @@ add_pseudo_headers [ "authority", "example.com"; "path", "/hello" ]
    @@ of_list [ "foo", "bar"; "qoo", "qux" ]
  in
  [%test_eq: String.t] actual expected
;;

(*H2.Headers.add ?sensitive h k v*)
let[@inline] replace ?sensitive k v h = H2.Headers.add ?sensitive h k v

let[@inline] upsert ?sensitive k v h =
  match get k h with
  | Some _ -> replace ?sensitive k v h
  | _ -> add ?sensitive k v h
;;

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
    let add = upsert ~sensitive:false attr
    let upsert = upsert ~sensitive:false attr
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

let default = Content_type.set_grpc_proto @@ of_list [ "te", "trailers" ]
