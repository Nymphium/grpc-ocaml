open Import

open struct
  module M = T.Op
  module D = M.Data
end

type ref_recv_initial_metadata = { metadata : T.Metadata.Array.t structure ptr }
type ref_recv_message = { message : T.Byte_buffer.t structure ptr ptr }

type ref_recv_status_on_client =
  { status : Status.Code.t ptr
  ; metadata : T.Metadata.Array.t structure ptr
  ; details : T.Slice.t structure ptr
  ; error_message : string ptr
  }

type ref_recv_close_on_server = { cancelled : int ptr }

type bwd =
  [ `Send_initial_metadata of Metadata.bwd
  | `Send_message of string
  | `Send_status_from_server of Status.t
  | `Send_close_from_client
  | `Recv_initial_metadata of ref_recv_initial_metadata
  | `Recv_message of ref_recv_message
  | `Recv_status_on_client of ref_recv_status_on_client
  | `Recv_close_on_server of ref_recv_close_on_server
  ]

let make_ref_initial_metadata () =
  let metadata = Metadata.allocate () in
  `Recv_initial_metadata { metadata }
;;

let make_ref_recv_message () =
  let message = malloc (ptr T.Byte_buffer.t) in
  `Recv_message { message }
;;

let make_ref_recv_status_on_client () =
  let status = Status.allocate () in
  let metadata = Metadata.allocate () in
  let details = malloc T.Slice.t in
  let error_message = malloc string in
  `Recv_status_on_client { status; metadata; details; error_message }
;;

let make_ref_recv_close_on_server () =
  let cancelled = malloc int in
  `Recv_close_on_server { cancelled }
;;

let to_op_type = function
  | `Send_initial_metadata _ -> M.Type.SEND_INITIAL_METADATA
  | `Send_message _ -> M.Type.SEND_MESSAGE
  | `Send_status_from_server _ -> M.Type.SEND_STATUS_FROM_SERVER
  | `Send_close_from_client -> M.Type.SEND_CLOSE_FROM_CLIENT
  | `Recv_initial_metadata _ -> M.Type.RECV_INITIAL_METADATA
  | `Recv_message _ -> M.Type.RECV_MESSAGE
  | `Recv_status_on_client _ -> M.Type.RECV_STATUS_ON_CLIENT
  | `Recv_close_on_server _ -> M.Type.RECV_CLOSE_ON_SERVER
;;

let update_status_from_server data status =
  (* TODO: rwite to md and Send_status_from_server POINTER? *)
  let status_details =
    Option.map (Ctypes.addr <@ Slice.from_static_string) status.Status.details
  in
  let metadata = Metadata.make status.metadata in
  let it = data |-> D.send_status_from_server in
  it |-> D.Send_status_from_server.status <-@ status.code;
  it |-> D.Send_status_from_server.status_details <-@ status_details;
  it
  |-> D.Send_status_from_server.trailing_metadata
  <-@ getf !@metadata T.Metadata.Array.metadata;
  it
  |-> D.Send_status_from_server.trailing_metadata_count
  <-@ Unsigned.Size_t.of_int @@ List.length status.metadata
;;

let make (bwd : bwd) =
  let op = Ctypes.make M.t in
  let data = op @. M.data in
  op @. M.reserved <-@ __reserved__;
  op @. M.op <-@ to_op_type bwd;
  op @. M.flags <-@ Unsigned.UInt32.zero;
  match bwd with
  | `Send_close_from_client | `Send_initial_metadata [] -> op
  | `Send_initial_metadata md ->
    let md' = Metadata.make md |->* T.Metadata.Array.metadata in
    let it = data |-> D.send_initial_metadata in
    let size = List.length md |> Unsigned.Size_t.of_int in
    it |-> D.Send_initial_metadata.metadata <-@ md';
    it |-> D.Send_initial_metadata.count <-@ size;
    op
  | `Send_message msg ->
    let msg' = Byte_buffer.from_string msg in
    let it = data |-> D.send_message in
    it |-> D.Send_message.send_message <-@ msg';
    op
  | `Send_status_from_server st ->
    let () = update_status_from_server data st in
    op
  | `Recv_initial_metadata ref ->
    let it = data |-> D.recv_initial_metadata in
    it |-> D.Recv_initial_metadata.recv_initial_metadata <-@ ref.metadata;
    op
  | `Recv_message ref ->
    let it = data |-> D.recv_message in
    it |-> D.Recv_message.recv_message <-@ ref.message;
    op
  | `Recv_status_on_client ref ->
    let it = data |-> D.recv_status_on_client in
    it |-> D.Recv_status_on_client.trailing_metadata <-@ ref.metadata;
    it |-> D.Recv_status_on_client.status <-@ ref.status;
    it |-> D.Recv_status_on_client.status_details <-@ Some ref.details;
    it |-> D.Recv_status_on_client.error_string <-@ Some ref.error_message;
    op
  | `Recv_close_on_server ref ->
    let it = data |-> D.recv_close_on_server in
    it |-> D.Recv_close_on_server.cancelled <-@ ref.cancelled;
    op
;;

let make_ops = List.map make
