open Import

open struct
  module M = T.Call
end

type send_metadata =
  { mu : Lwt_mutex.t
  ; md : Metadata.bwd
  ; mutable sent : bool
  }

(** wrapped call data *)
type t =
  { call : M.t
  ; cq : T.Completion.Queue.t
  ; flags : T.Flags.Write.t
  ; send_metadata : send_metadata
  }

type write_flag =
  [ `Buffer_hit
  | `No_compress
  ]

module Batch_stack = struct
  type t

  let t : t structure typ = Ctypes.structure "grpc_ocaml_run_batch_stack"

  let recv_initial_metadata =
    Ctypes.field t "recv_initial_metadata" @@ ptr T.Metadata.Array.t
  ;;

  let recv_message = Ctypes.field t "recv_message" @@ ptr (ptr T.Byte_buffer.t)
  let status = Ctypes.field t "recv_status" @@ ptr T.Status_code.t
  let tr = Ctypes.field t "tr" @@ ptr T.Metadata.Array.t
  let details = Ctypes.field t "details" @@ ptr T.Slice.t
  let error_message = Ctypes.field t "error_message" @@ ptr Ctypes.string
  let cancelled = Ctypes.field t "cancelled" @@ ptr int
  let () = seal t
  let destroy = free

  let make_tag_pair () =
    let t = malloc t in
    let tag = to_voidp t in
    t, tag
  ;;
end

module Details = struct
  let init = F.Call.Details.init

  let allocate () =
    let ptr = malloc ~finalise:F.Call.Details.destroy T.Call_details.t in
    init ptr;
    ptr
  ;;

  let destroy = F.Call.Details.destroy
end

let make_send_metadata ?(md = []) () =
  let mu = Lwt_mutex.create () in
  { md; mu; sent = false }
;;

let wrap_raw
    ?(flags = Propagation_bits.defaults)
    ?(send_metadata = make_send_metadata ())
    ~cq
    ~call
    ()
  =
  { call; cq; flags; send_metadata }
;;

let destroy { call; cq; _ } =
  F.Call.unref call;
  F.Completion_queue.destroy cq
;;

let allocate () = malloc M.t

(** @parm flags TODO: investigate write flag and/or propagate flag? *)
let make
    ~channel
    ?parent
    ?flags
    ~methd
    ?(host = Channel.get_target channel)
    ?(deadline = Timespec.inf_future')
    ()
  =
  let flags =
    match flags with
    | Some f -> Unsigned.UInt32.of_int f
    | None -> Unsigned.UInt32.zero
  in
  let host_slice = Slice.from_static_string host in
  let parent_call =
    match parent with
    | None -> to_voidp null
    | Some parent -> parent.call
  in
  let cq = F.Completion_queue.create_for_pluck __reserved__ in
  let methd_slice = Slice.from_static_string methd in
  let bg = Channel.get_bg channel in
  let call =
    F.Channel.create_call
      bg.channel
      parent_call
      flags
      cq
      methd_slice
      (Ctypes.addr host_slice)
      deadline
      __reserved__
  in
  let () =
    if Ctypes.(is_null @@ to_voidp call)
    then
      failwith
      @@ Printf.sprintf "cannot create call with method %s"
      @@ Slice.to_string methd_slice
  in
  let () = Slice.unref methd_slice in
  let () = Slice.unref host_slice in
  wrap_raw ~cq ~call ~flags ()
;;

let run_batch ?(tag = Ctypes.null) t ops =
  let ops_size = List.length ops in
  let ops_size' = Unsigned.Size_t.of_int ops_size in
  let ops =
    (CArray.(start <@ of_list T.Op.t)
    <@ List.map (fun op ->
           op @. T.Op.flags <-@ t.flags;
           op))
      (Op.make_ops ops)
  in
  let err = F.Call.start_batch t.call ops ops_size' tag __reserved__ in
  let () =
    if err <> M.Error.OK
    then failwith @@ Printf.sprintf "prepare call error(%s)" @@ M.Error.show err
  in
  let inf = Timespec.(inf_future Clock_type.realtime) in
  let%lwt ev = Completion_queue.pluck t.cq inf tag in
  if ev @.* T.Event.success < 1
  then (
    let st = ev @.* T.Event.typ in
    Lwt.fail_with @@ Printf.sprintf "run_batch failed(%s)" @@ T.Completion.Type.show st)
  else Lwt.return (ops, ops_size)
;;

let unary_request ?(metadata = []) ?message t : string option Protoiso.res Lwt.t =
  let (`Recv_initial_metadata recv_initial_metadata as rim) =
    Op.make_ref_initial_metadata ()
  in
  let (`Recv_message recv_message as rm) = Op.make_ref_recv_message () in
  let (`Recv_status_on_client recv_status_on_client as rsoc) =
    Op.make_ref_recv_status_on_client ()
  in
  let ops =
    let it =
      [ `Send_initial_metadata metadata; `Send_close_from_client; rim; rm; rsoc ]
    in
    let add_msg =
      match message with
      | Some (msg : string) -> List.cons (`Send_message msg)
      | None -> Fun.id
    in
    add_msg it
  in
  let stack, tag = Batch_stack.make_tag_pair () in
  stack |-> Batch_stack.recv_message <-@ recv_message.message;
  stack |-> Batch_stack.recv_initial_metadata <-@ recv_initial_metadata.metadata;
  stack |-> Batch_stack.status <-@ recv_status_on_client.status;
  stack |-> Batch_stack.details <-@ recv_status_on_client.details;
  stack |-> Batch_stack.error_message <-@ recv_status_on_client.error_message;
  stack |-> Batch_stack.tr <-@ recv_status_on_client.metadata;
  let%lwt _o = run_batch ~tag t ops in
  let status = !@(recv_status_on_client.status) in
  let md =
    let init_md = Metadata.to_bwd recv_initial_metadata.metadata in
    let md = Metadata.to_bwd recv_status_on_client.metadata in
    md @ init_md
  in
  let status = Status.Code.to_bwd status in
  match status with
  | `OK ->
    let recv_message = Byte_buffer.to_string_opt !@(recv_message.message) in
    Lwt_result.return (recv_message, md)
  | #Status.Code.fail_bwd as status ->
    let details =
      let slice = !@(recv_status_on_client.details) in
      if is_null @@ Slice.start_ptr slice then None else Some (Slice.to_string slice)
    in
    Lwt_result.fail (status, details, md)
;;

let remote_read t is_metadata_received =
  let (`Recv_message recv_message as rm) = Op.make_ref_recv_message () in
  let (`Recv_initial_metadata recv_initial_metadata as rim) =
    Op.make_ref_initial_metadata ()
  in
  let ops = if is_metadata_received then [ rm; rim ] else [ rm ] in
  let stack, tag = Batch_stack.make_tag_pair () in
  stack |-> Batch_stack.recv_message <-@ recv_message.message;
  stack |-> Batch_stack.recv_initial_metadata <-@ recv_initial_metadata.metadata;
  let%lwt _ = run_batch ~tag t ops in
  let () = Batch_stack.destroy stack in
  let md = Metadata.to_bwd recv_initial_metadata.metadata in
  Lwt.return @@ (Byte_buffer.to_string_opt !@(recv_message.message), md)
;;

let unary_response ?(code = `OK) ?details ?(md = []) ?(tr = []) t req =
  let code = Status.Code.of_bwd code in
  let (`Recv_close_on_server recv_close_on_server as rcos) =
    Op.make_ref_recv_close_on_server ()
  in
  let ops =
    let add_msg =
      match req with
      | Some req -> List.cons (`Send_message req)
      | None -> Fun.id
    in
    add_msg
      [ `Send_initial_metadata md
      ; `Send_status_from_server Status.{ code; details; metadata = tr }
      ; rcos
      ]
  in
  let stack = malloc Batch_stack.t in
  let tag = Ctypes.to_voidp @@ stack in
  stack |-> Batch_stack.cancelled <-@ recv_close_on_server.cancelled;
  let%lwt _ = run_batch ~tag t ops in
  let closed_on_server = deref @@ recv_close_on_server.cancelled in
  if closed_on_server <= 0
  then Log.message __FILE__ __LINE__ `Info "not (properly) closed request by server";
  Lwt.return_unit
;;

(** return `DEADLINE_EXCEEDED` response to its call *)
let deadline_exceeded t = unary_response ~code:`DEADLINE_EXCEEDED t None

(** exec io with deadline:
  - if `deadline` is `inf_future'` then just wait until `io` is completed
  - else if `deadline` is `inf_past'` then returns `DEADLINE_EXCEEDED` response
  - otherwise wait `io` until `deadline` is exceeded
  *)
let with_timeout t deadline io =
  if Timespec.cmp deadline Timespec.inf_future' = 0
  then io
  else if Timespec.cmp deadline Timespec.inf_past' = 0
  then deadline_exceeded t
  else
    Lwt.pick
      [ (let sec =
           Timespec.(to_micros @@ sub deadline (now Clock_type.realtime)) /. 1000_000.
         in
         let%lwt () = Lwt_unix.sleep sec in
         deadline_exceeded t)
      ; io
      ]
;;