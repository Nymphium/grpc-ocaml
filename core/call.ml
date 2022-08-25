open Import
include T.Call

type raw = t

let raw = t

module Batch_stack = struct
  type t

  let t : t structure typ = Ctypes.structure "grpc_ocaml_run_batch_stack"
  let recv_initial_metadata = Ctypes.field t "recv_initial_metadata" @@ ptr Metadata.raw
  let recv_message = Ctypes.field t "recv_message" @@ ptr (ptr Byte_buffer.raw)
  let status = Ctypes.field t "recv_status" @@ ptr Status.Code.t
  let tr = Ctypes.field t "tr" @@ ptr Metadata.raw
  let details = Ctypes.field t "details" @@ ptr Slice.t
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
  include T.Call_details

  let init = F.Call.Details.init

  let allocate () =
    let t = malloc t in
    init t;
    t
  ;;

  let destroy = F.Call.Details.destroy
end

(** wrapped call data *)
type t =
  { call : raw
  ; cq : Completion_queue.raw
  ; flags : T.Flags.Write.t
  }

type write_flag =
  [ `Buffer_hit
  | `No_compress
  ]

let destroy { call; cq; _ } =
  F.Call.unref call;
  F.Completion_queue.destroy cq
;;

let wrap_raw ?(flags = Propagation_bits.defaults) ~cq ~call () = { call; cq; flags }
let allocate () = malloc t

(** make call to ["${host}/${method}"], which [host] is [get_target channel] by default.
    If [parent] is not empty, the call is a child call of [parent].
    [deadline] is inf_future by default.
   @parm flags TODO: investigate write flag and/or propagate flag? *)
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
  let host_slice = Slice.from_string host in
  let parent_call =
    match parent with
    | None -> to_voidp null
    | Some parent -> parent.call
  in
  let cq = Completion_queue.create_for_pluck () in
  let methd_slice = Slice.from_string methd in
  let call =
    F.Channel.create_call
      channel.channel
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
    if err <> Error.OK
    then failwith @@ Printf.sprintf "prepare call error(%s)" @@ Error.show err
  in
  let inf = Timespec.(inf_future Clock_type.realtime) in
  let ev = Completion_queue.pluck t.cq inf tag in
  if ev @.* T.Event.success < 1
  then (
    let st = ev @.* T.Event.typ in
    failwith @@ Printf.sprintf "run_batch failed(%s)" @@ Completion_queue.Type.show st)
  else ops, ops_size
;;

let unary_request ?(metadata = []) ?message t =
  let (`Recv_initial_metadata recv_initial_metadata' as rim) =
    Op.make_ref_initial_metadata ()
  in
  let (`Recv_message recv_message' as rm) = Op.make_ref_recv_message () in
  let (`Recv_status_on_client recv_status_on_client' as rsoc) =
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
  Batch_stack.(
    stack |-> recv_message <-@ recv_message'.message;
    stack |-> recv_initial_metadata <-@ recv_initial_metadata'.metadata;
    stack |-> status <-@ recv_status_on_client'.status;
    stack |-> details <-@ recv_status_on_client'.details;
    stack |-> error_message <-@ recv_status_on_client'.error_message;
    stack |-> tr <-@ recv_status_on_client'.metadata);
  let ops, len = run_batch ~tag t ops in
  let status = !@(recv_status_on_client'.status) in
  let md =
    let init_md = Metadata.to_bwd recv_initial_metadata'.metadata in
    let md = Metadata.to_bwd recv_status_on_client'.metadata in
    md @ init_md
  in
  let status = Status.Code.to_bwd status in
  match status with
  | `OK ->
    let recv_message = Byte_buffer.to_string_opt !@(recv_message'.message) in
    let () =
      Batch_stack.destroy stack;
      Op.destroy ops len
    in
    Ok (recv_message, md)
  | #Status.Code.fail_bwd as status ->
    let details =
      let slice = !@(recv_status_on_client'.details) in
      if Slice.is_empty slice then None else Some (Slice.to_string slice)
    in
    let () =
      Batch_stack.destroy stack;
      Op.destroy ops len
    in
    Error (status, details, md)
;;

let remote_read t =
  let (`Recv_message recv_message as rm) = Op.make_ref_recv_message () in
  let ops = [ rm ] in
  let stack, tag = Batch_stack.make_tag_pair () in
  stack |-> Batch_stack.recv_message <-@ recv_message.message;
  let ops, len = run_batch ~tag t ops in
  let message = Byte_buffer.to_string_opt !@(recv_message.message) in
  let () =
    Batch_stack.destroy stack;
    Op.destroy ops len
  in
  message
;;

(** send unary response: run RECV_CLOSE_ON_SERVER, SEND_INITIAL_METADATA and SEND_STATUS_FROM_SERVER ops *)
let unary_response ?(code = `OK) ?details ?(md = []) ?(tr = []) t res =
  let code = Status.Code.of_bwd code in
  let (`Recv_close_on_server recv_close_on_server as rcos) =
    Op.make_ref_recv_close_on_server ()
  in
  let ops =
    let add_msg =
      match res with
      | Some res -> List.cons (`Send_message res)
      | None -> Fun.id
    in
    add_msg
      [ `Send_initial_metadata md
      ; `Send_status_from_server Status.{ code; details; metadata = tr }
      ; rcos
      ]
  in
  let stack, tag = Batch_stack.make_tag_pair () in
  stack |-> Batch_stack.cancelled <-@ recv_close_on_server.cancelled;
  let ops, len = run_batch ~tag t ops in
  let closed_on_server = deref @@ recv_close_on_server.cancelled in
  let () =
    Batch_stack.destroy stack;
    Op.destroy ops len
  in
  if closed_on_server <= 0
  then Log.message __FILE__ __LINE__ `Info "not (properly) closed request by server"
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
  then Lwt.return @@ deadline_exceeded t
  else
    Lwt.pick
      [ (let sec =
           Timespec.(to_micros @@ sub deadline (now Clock_type.realtime)) /. 1000_000.
         in
         let%lwt () = Lwt_unix.sleep sec in
         Lwt.return @@ deadline_exceeded t)
      ; io
      ]
;;
