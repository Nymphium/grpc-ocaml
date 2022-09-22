open Import
include T.Call

type raw = t

let raw = t

module Batch_stack = struct
  type t

  let t : t structure typ = Ctypes.structure "grpc_ocaml_run_batch_stack"
  let ops = Ctypes.field t "ops" @@ ptr Op.t
  let ops_num = Ctypes.field t "ops_num" int
  let send_metadata = Ctypes.field t "send_metadata" Metadata.t
  let send_trailing_metadata = Ctypes.field t "send_trailing_metadata" Metadata.t
  let send_status_details = Ctypes.field t "send_status_details" Slice.t
  let recv_initial_metadata = Ctypes.field t "recv_initial_metadata" Metadata.t
  let recv_message = Ctypes.field t "recv_message" @@ ptr Byte_buffer.raw
  let status = Ctypes.field t "recv_status" @@ Status.Code.t
  let recv_trailing_metadata = Ctypes.field t "recv_trailing_metadata" Metadata.t
  let recv_status_details = Ctypes.field t "recv_status_details" Slice.t
  let error_message = Ctypes.field t "error_message" string
  let cancelled = Ctypes.field t "cancelled" int
  let () = seal t

  let destroy t =
    (* let destroy_md_with_entries md = *)
    (* Metadata.destroy_entries *)
    (* (md |->* Metadata.metadata) *)
    (* (md |->* Metadata.count |> Unsigned.Size_t.to_int); *)
    (* Metadata.destroy md *)
    (* in *)
    (* destroy_md_with_entries (t |-> send_metadata); *)
    (* destroy_md_with_entries (t |-> send_trailing_metadata); *)
    Metadata.destroy (t |-> recv_initial_metadata);
    Metadata.destroy (t |-> recv_trailing_metadata)
  ;;

  (* let recv_status_details = t |->* recv_status_details in *)
  (* if Slice.start_ptr recv_status_details |> (not <@ is_null) *)
  (* then Slice.unref recv_status_details *)

  (* let recv_message = t |->* recv_message in *)
  (* if not @@ is_null recv_message then Byte_buffer.destroy recv_message; *)
  (* CArray.from_ptr (t |->* ops) (t |->* ops_num) *)
  (* |> CArray.iter *)
  (* @@ fun op' -> *)
  (* match op' @.* Op.op with *)
  (* | Op.Type.SEND_MESSAGE -> *)
  (* Byte_buffer.destroy *)
  (* (op' @. Op.data |-> Op.Data.send_message |->* Op.Data.Send_message.send_message) *)
  (* | _ -> () *)

  let make_tag_pair () =
    let t = malloc t in
    Metadata.init (t |-> send_metadata);
    Metadata.init (t |-> send_trailing_metadata);
    Metadata.init (t |-> recv_initial_metadata);
    Metadata.init (t |-> recv_trailing_metadata);
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

let run_batch t ?(tag = null) ops ops_num =
  let err =
    F.Call.start_batch t.call ops (Unsigned.Size_t.of_int ops_num) tag __reserved__
  in
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
;;

let unary_request ?(metadata = []) ?message t =
  let ops =
    let it =
      [ `Send_initial_metadata metadata
      ; `Send_close_from_client
      ; `Recv_initial_metadata
      ; `Recv_message
      ; `Recv_status_on_client
      ]
    in
    let add_msg =
      match message with
      | Some (msg : string) -> List.cons (`Send_message msg)
      | None -> Fun.id
    in
    add_msg it
  in
  let ops' = Op.make_ops ops t.flags in
  let ops_num = List.length ops in
  let recv_message =
    Op.(
      get ops' ops_num `Recv_message @. Data.recv_message
      |-> Data.Recv_message.recv_message)
  in
  let recv_initial_metadata =
    Op.(get ops' ops_num `Recv_initial_metadata @. Data.recv_initial_metadata)
  in
  let recv_status_on_client =
    Op.(get ops' ops_num `Recv_status_on_client @. Data.recv_status_on_client)
  in
  let md =
    recv_initial_metadata |-> Op.Data.Recv_initial_metadata.recv_initial_metadata
  in
  let status = recv_status_on_client |-> Op.Data.Recv_status_on_client.status in
  let status_details =
    recv_status_on_client |-> Op.Data.Recv_status_on_client.status_details
  in
  let error_string =
    recv_status_on_client |-> Op.Data.Recv_status_on_client.error_string
  in
  let tr = recv_status_on_client |-> Op.Data.Recv_status_on_client.trailing_metadata in
  let stack, tag = Batch_stack.make_tag_pair () in
  stack |-> Batch_stack.ops <-@ ops';
  stack |-> Batch_stack.ops_num <-@ ops_num;
  recv_message <-@ (stack |-> Batch_stack.recv_message);
  md <-@ (stack |-> Batch_stack.recv_initial_metadata);
  status <-@ (stack |-> Batch_stack.status);
  status_details <-@ (stack |-> Batch_stack.recv_status_details);
  error_string <-@ (stack |-> Batch_stack.error_message);
  tr <-@ (stack |-> Batch_stack.recv_trailing_metadata);
  let () = run_batch ~tag t ops' ops_num in
  let status = !@status in
  let md =
    let md = Metadata.to_bwd !@md in
    let tr = Metadata.to_bwd !@tr in
    tr @ md
  in
  let status = Status.Code.to_bwd !@status in
  Fun.protect ~finally:(fun () -> Batch_stack.destroy stack)
  @@ fun () ->
  match status with
  | `OK ->
    let recv_message = Byte_buffer.to_string_opt !@(!@recv_message) in
    Ok (recv_message, md)
  | #Status.Code.fail_bwd as status ->
    let details =
      let slice = !@status_details in
      if is_null slice then None else Some (Slice.to_string !@slice)
    in
    Error (status, details, md)
;;

let remote_read t =
  let ops = [ `Recv_message ] in
  let ops_num = 1 in
  let ops' = Op.make_ops ops t.flags in
  let recv_message =
    Op.(
      get ops' ops_num `Recv_message @. Data.recv_message
      |-> Data.Recv_message.recv_message)
  in
  let stack, tag = Batch_stack.make_tag_pair () in
  stack |-> Batch_stack.ops <-@ ops';
  stack |-> Batch_stack.ops_num <-@ ops_num;
  recv_message <-@ (stack |-> Batch_stack.recv_message);
  Fun.protect ~finally:(fun () -> Batch_stack.destroy stack)
  @@ fun () ->
  let () = run_batch ~tag t ops' ops_num in
  let message = Byte_buffer.to_string_opt !@(!@recv_message) in
  message
;;

(** send unary response: run RECV_CLOSE_ON_SERVER, SEND_INITIAL_METADATA and SEND_STATUS_FROM_SERVER ops *)
let unary_response ?(code = `OK) ?details ?(md = []) ?(tr = []) t res =
  let code = Status.Code.of_bwd code in
  let ops =
    let add_msg =
      match res with
      | Some res -> List.cons (`Send_message res)
      | None -> Fun.id
    in
    add_msg
      [ `Send_initial_metadata md
      ; `Send_status_from_server Status.{ code; details; metadata = tr }
      ; `Recv_close_on_server
      ]
  in
  let ops_num = List.length ops in
  let ops' = Op.make_ops ops t.flags in
  let stack, tag = Batch_stack.make_tag_pair () in
  let send_initial_metadata =
    Op.(get ops' ops_num `Send_initial_metadata @. Data.send_initial_metadata)
  in
  let send_status_from_server =
    Op.(get ops' ops_num `Send_status_from_server @. Data.send_status_from_server)
  in
  let send_md = send_initial_metadata |-> Op.Data.Send_initial_metadata.metadata in
  let send_md_count = send_initial_metadata |-> Op.Data.Send_initial_metadata.count in
  let send_metadata = stack |-> Batch_stack.send_metadata in
  send_metadata |-> Metadata.metadata <-@ !@send_md;
  send_metadata |-> Metadata.count <-@ !@send_md_count;
  (* let tr'' = Metadata.make tr in *)
  let send_tr =
    send_status_from_server |->* Op.Data.Send_status_from_server.trailing_metadata
  in
  let send_tr_count =
    send_status_from_server |->* Op.Data.Send_status_from_server.trailing_metadata_count
  in
  let send_trailing_metadata = stack |-> Batch_stack.send_trailing_metadata in
  send_trailing_metadata |-> Metadata.metadata <-@ send_tr;
  send_trailing_metadata |-> Metadata.count <-@ send_tr_count;
  let recv_close_on_server =
    Op.(get ops' ops_num `Recv_close_on_server @. Data.recv_close_on_server)
  in
  let cancelled = recv_close_on_server |-> Op.Data.Recv_close_on_server.cancelled in
  stack |-> Batch_stack.ops <-@ ops';
  stack |-> Batch_stack.ops_num <-@ ops_num;
  cancelled <-@ (stack |-> Batch_stack.cancelled);
  Fun.protect ~finally:(fun () -> Batch_stack.destroy stack)
  @@ fun () ->
  let () = run_batch ~tag t ops' ops_num in
  let closed_on_server = !@(!@cancelled) in
  if closed_on_server <= 0
  then Log.message __FILE__ __LINE__ `Info "not (properly) closed request by server"
;;

(** return [DEADLINE_EXCEEDED] response to its call *)
let deadline_exceeded t = unary_response ~code:`DEADLINE_EXCEEDED t None

(** exec io with deadline:
  - return [Some a] when [io] is completed before [deadline] exceeded
  - if [deadline] is [inf_future'] then just wait until [io] is completed
  - else if [deadline] is [inf_past'] then returns [DEADLINE_EXCEEDED] response
  - otherwise wait [io] until [deadline] is exceeded
  *)
let with_timeout t deadline io =
  if Timespec.cmp deadline Timespec.inf_future' = 0
  then io >|= Option.some
  else if Timespec.cmp deadline Timespec.inf_past' = 0
  then (
    deadline_exceeded t;
    Lwt.return_none)
  else
    Lwt.pick
      [ (let sec =
           Timespec.(to_micros @@ sub deadline (now Clock_type.realtime)) /. 1000_000.
         in
         let%lwt () = Lwt_unix.sleep sec in
         deadline_exceeded t;
         Lwt.return_none)
      ; io >|= Option.some
      ]
;;
