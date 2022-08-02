open Import

open struct
  module M = T.Op
  module D = M.Data
end

type fwd =
  [ `Send_initial_metadata of Metadata.fwd
  | `Send_message of string
  | `Send_status_from_server of Status.t
  | `Send_close_from_client
  | `Recv_initial_metadata
  | `Recv_message
  | `Recv_status_on_client
  | `Recv_close_on_server
  ]

let to_op_type = function
  | `Send_initial_metadata _ -> M.Type.SEND_INITIAL_METADATA
  | `Send_message _ -> M.Type.SEND_MESSAGE
  | `Send_status_from_server _ -> M.Type.SEND_STATUS_FROM_SERVER
  | `Send_close_from_client -> M.Type.SEND_CLOSE_FROM_CLIENT
  | `Recv_initial_metadata -> M.Type.RECV_INITIAL_METADATA
  | `Recv_message -> M.Type.RECV_MESSAGE
  | `Recv_status_on_client -> M.Type.RECV_STATUS_ON_CLIENT
  | `Recv_close_on_server -> M.Type.RECV_CLOSE_ON_SERVER
;;

let update_status_from_server op status =
  (* TODO: rwite to md and Send_status_from_server POINTER? *)
  let%lwt () =
    let%lwt status_details =
      match status.Status.details with
      | Some details ->
        Lwt.map (fun v -> Some (Ctypes.addr v)) @@ Slice.from_static_string details
      | None -> Lwt.return None
    in
    let%lwt metadata = Metadata.make status.metadata in
    let data =
      let it = Ctypes.make D.t in
      let send_status_from_server =
        let it = Ctypes.make D.Send_status_from_server.t in
        it @. D.Send_status_from_server.status <-@ status.Status.code;
        it @. D.Send_status_from_server.status_details <-@ status_details;
        it @. D.Send_status_from_server.trailing_metadata
        <-@ deref @@ metadata @. T.Metadata.Array.metadata;
        it
      in
      it @. D.send_status_from_server <-@ send_status_from_server;
      it
    in
    op @. M.data <-@ data;
    Lwt.return_unit
  in
  Lwt.return_unit
;;

let make (fwd : fwd) =
  let op = Ctypes.make M.t in
  op @. M.op <-@ to_op_type fwd;
  op @. M.flags <-@ Unsigned.UInt32.zero;
  match fwd with
  | `Send_initial_metadata md ->
    let%lwt md' =
      (fun a -> deref @@ a @. T.Metadata.Array.metadata) =|< Metadata.make md
    in
    let data =
      let it = Ctypes.make D.t in
      let send_initial_metadata =
        let it = Ctypes.make D.Send_initial_metadata.t in
        let size = List.length md |> Unsigned.Size_t.of_int in
        it @. D.Send_initial_metadata.metadata <-@ md';
        it @. D.Send_initial_metadata.count <-@ size;
        it
      in
      it @. D.send_initial_metadata <-@ send_initial_metadata;
      it
    in
    op @. M.data <-@ data;
    Lwt.return op
  | `Send_message msg ->
    let%lwt msg' = Byte_buffer.from_string msg in
    let data =
      let it = Ctypes.make D.t in
      let send_message =
        let it = Ctypes.make D.Send_message.t in
        it @. D.Send_message.send_message <-@ msg';
        it
      in
      it @. D.send_message <-@ send_message;
      it
    in
    op @. M.data <-@ data;
    Lwt.return op
  | `Send_status_from_server st ->
    let%lwt () = update_status_from_server op st in
    Lwt.return op
  | `Recv_initial_metadata ->
    let%lwt md = Ctypes.addr =|< Metadata.make [] in
    let data =
      let it = Ctypes.make D.t in
      let recv_initial_metadata = Ctypes.make D.Recv_initial_metadata.t in
      recv_initial_metadata @. D.Recv_initial_metadata.recv_initial_metadata <-@ md;
      it @. D.recv_initial_metadata <-@ recv_initial_metadata;
      it
    in
    op @. M.data <-@ data;
    Lwt.return op
  | `Send_close_from_client ->
    let data = Ctypes.make D.t in
    op @. M.data <-@ data;
    Lwt.return op
  | `Recv_message ->
    let data = Ctypes.make D.t in
    let it = Ctypes.make D.Recv_message.t in
    let%lwt msg = Byte_buffer.from_string "" in
    it @. D.Recv_message.recv_message <-@ msg;
    data @. D.recv_message <-@ it;
    op @. M.data <-@ data;
    Lwt.return op
  | `Recv_status_on_client ->
    let data = Ctypes.make D.t in
    let it = Ctypes.make D.Recv_status_on_client.t in
    data @. D.recv_status_on_client <-@ it;
    op @. M.data <-@ data;
    Lwt.return op
  | `Recv_close_on_server ->
    let data = Ctypes.make D.t in
    let it = Ctypes.make D.Recv_close_on_server.t in
    data @. D.recv_close_on_server <-@ it;
    op @. M.data <-@ data;
    Lwt.return op
;;

let make_ops = Lwt.all <@ List.map make
