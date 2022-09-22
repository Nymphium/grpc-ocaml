open Import
include T.Op

type raw = t

let raw = t

type bwd =
  [ `Send_initial_metadata of Metadata.bwd
  | `Send_message of string
  | `Send_status_from_server of Status.t
  | `Send_close_from_client
  | `Recv_initial_metadata
  | `Recv_message
  | `Recv_status_on_client
  | `Recv_close_on_server
  ]

let to_op_type = function
  | `Send_initial_metadata _ -> Type.SEND_INITIAL_METADATA
  | `Send_message _ -> Type.SEND_MESSAGE
  | `Send_status_from_server _ -> Type.SEND_STATUS_FROM_SERVER
  | `Send_close_from_client -> Type.SEND_CLOSE_FROM_CLIENT
  | `Recv_initial_metadata -> Type.RECV_INITIAL_METADATA
  | `Recv_message -> Type.RECV_MESSAGE
  | `Recv_status_on_client -> Type.RECV_STATUS_ON_CLIENT
  | `Recv_close_on_server -> Type.RECV_CLOSE_ON_SERVER
;;

let destroy ops len =
  let ops = CArray.(from_ptr ops len) in
  flip CArray.iter ops
  @@ fun op' ->
  let data = op' @. data in
  let typ = op' @.* op in
  match typ with
  | Type.SEND_CLOSE_FROM_CLIENT | Type.RECV_CLOSE_ON_SERVER | Type.SEND_MESSAGE -> ()
  | Type.SEND_INITIAL_METADATA ->
    let it = data |-> Data.send_initial_metadata in
    Data.Send_initial_metadata.(
      let md = it |->* metadata in
      let len = it |->* count |> Unsigned.Size_t.to_int in
      Metadata.destroy_entries md len)
  | Type.SEND_STATUS_FROM_SERVER ->
    let it = data |-> Data.send_status_from_server in
    Data.Send_status_from_server.(
      let () =
        let md = it |->* trailing_metadata in
        let len = it |->* trailing_metadata_count |> Unsigned.Size_t.to_int in
        Metadata.destroy_entries md len
      in
      let status_details = it |->* status_details in
      if not @@ is_null status_details then Slice.unref !@status_details)
  | Type.RECV_INITIAL_METADATA ->
    let it = data |-> Data.recv_initial_metadata in
    let md' = it |->* Data.Recv_initial_metadata.recv_initial_metadata in
    let md = md' |->* Metadata.metadata in
    let len = md' |->* Metadata.count |> Unsigned.Size_t.to_int in
    Metadata.destroy_entries md len;
    Metadata.destroy md'
  | Type.RECV_MESSAGE ->
    let it = data |-> Data.recv_message in
    let recv_message = !@(it |->* Data.Recv_message.recv_message) in
    if not @@ is_null recv_message then Byte_buffer.destroy recv_message
  | Type.RECV_STATUS_ON_CLIENT ->
    let it = data |-> Data.recv_status_on_client in
    Data.Recv_status_on_client.(
      Metadata.(
        let md = it |->* trailing_metadata in
        let md' = md |->* metadata in
        let len = md |->* count |> Unsigned.Size_t.to_int in
        destroy_entries md' len;
        destroy md);
      let error_string = it |-> error_string in
      F.Alloc.free (to_voidp error_string);
      let status_details = it |->* status_details in
      if not @@ is_null status_details then Slice.unref !@status_details)
;;

let make (bwd : bwd) =
  let t = Ctypes.make t in
  let data = t @. data in
  t @. reserved <-@ __reserved__;
  t @. op <-@ to_op_type bwd;
  t @. flags <-@ Unsigned.UInt32.zero;
  let () =
    match bwd with
    | `Send_close_from_client | `Send_initial_metadata [] -> ()
    | `Send_initial_metadata md ->
      let md' = Metadata.(make md |->* metadata) in
      let it = data |-> Data.send_initial_metadata in
      let size = List.length md |> Unsigned.Size_t.of_int in
      Data.Send_initial_metadata.(
        it |-> metadata <-@ md';
        it |-> count <-@ size)
    | `Send_message msg ->
      let msg' = Byte_buffer.from_string ~copy:true msg in
      let it = data |-> Data.send_message in
      it |-> Data.Send_message.send_message <-@ msg'
    | `Send_status_from_server st ->
      let status_details' =
        match st.Status.details with
        | Some details -> (Ctypes.addr <@ Slice.from_string ~copy:true) details
        | None -> from_voidp T.Slice.t null
      in
      let metadata = Metadata.(make st.metadata |->* metadata) in
      let it = data |-> Data.send_status_from_server in
      Data.Send_status_from_server.(
        it |-> status <-@ st.code;
        it |-> status_details <-@ status_details';
        it |-> trailing_metadata <-@ metadata;
        it
        |-> trailing_metadata_count
        <-@ Unsigned.Size_t.of_int @@ List.length st.metadata)
    | `Recv_initial_metadata ->
      let it = data |-> Data.recv_initial_metadata in
      let metadata = Metadata.allocate () in
      it |-> Data.Recv_initial_metadata.recv_initial_metadata <-@ metadata
    | `Recv_status_on_client ->
      let it = data |-> Data.recv_status_on_client in
      let tr = Metadata.allocate () in
      Data.Recv_status_on_client.(it |-> trailing_metadata <-@ tr)
    | _ -> ()
  in
  t
;;

let make_ops ops write_flag =
  let ops' = List.map make ops in
  List.iter
    (fun op' ->
      match op' @.* op with
      | Type.SEND_MESSAGE -> op' @. flags <-@ write_flag
      | _ -> ())
    ops';
  CArray.(start <@ of_list T.Op.t) ops'
;;

let get ops len typ =
  let r = ref None in
  let open CArray in
  (let exception Break in
  try
    from_ptr ops len
    |> iter
       @@ fun op' ->
       let data = op' @.* data in
       r := Some data;
       match op' @.* op with
       | Type.SEND_INITIAL_METADATA when typ = `Send_initial_metadata -> raise Break
       | Type.SEND_MESSAGE when typ = `Send_message ->
         r := Some data;
         raise Break
       | Type.SEND_STATUS_FROM_SERVER when typ = `Send_status_from_server -> raise Break
       | Type.SEND_CLOSE_FROM_CLIENT when typ = `Send_close_from_client -> raise Break
       | Type.RECV_INITIAL_METADATA when typ = `Recv_initial_metadata -> raise Break
       | Type.RECV_MESSAGE when typ = `Recv_message -> raise Break
       | Type.RECV_STATUS_ON_CLIENT when typ = `Recv_status_on_client -> raise Break
       | Type.RECV_CLOSE_ON_SERVER when typ = `Recv_close_on_server -> raise Break
       | _ -> ()
  with
  | Break -> ());
  match !r with
  | Some v -> v
  | None -> failwith "failed to get metadata"
;;
