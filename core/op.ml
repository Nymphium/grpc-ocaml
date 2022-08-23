open Import
include T.Op

type raw = t

let raw = t

type ref_recv_initial_metadata = { metadata : Metadata.raw structure ptr }
type ref_recv_message = { message : Byte_buffer.raw structure ptr ptr }

type ref_recv_status_on_client =
  { status : Status.Code.t ptr
  ; metadata : Metadata.raw structure ptr
  ; details : Slice.raw structure ptr
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
  let message = malloc (ptr Byte_buffer.t) in
  `Recv_message { message }
;;

let make_ref_recv_status_on_client () =
  let status = Status.allocate () in
  let metadata = Metadata.allocate () in
  let details = malloc Slice.t in
  let error_message = malloc string in
  `Recv_status_on_client { status; metadata; details; error_message }
;;

let make_ref_recv_close_on_server () =
  let cancelled = malloc int in
  `Recv_close_on_server { cancelled }
;;

let to_op_type = function
  | `Send_initial_metadata _ -> Type.SEND_INITIAL_METADATA
  | `Send_message _ -> Type.SEND_MESSAGE
  | `Send_status_from_server _ -> Type.SEND_STATUS_FROM_SERVER
  | `Send_close_from_client -> Type.SEND_CLOSE_FROM_CLIENT
  | `Recv_initial_metadata _ -> Type.RECV_INITIAL_METADATA
  | `Recv_message _ -> Type.RECV_MESSAGE
  | `Recv_status_on_client _ -> Type.RECV_STATUS_ON_CLIENT
  | `Recv_close_on_server _ -> Type.RECV_CLOSE_ON_SERVER
;;

let destroy ops len =
  let ops = CArray.(from_ptr ops len) in
  flip CArray.iter ops
  @@ fun op' ->
  let data = op' @. data in
  let typ = op' @.* op in
  match typ with
  | Type.SEND_CLOSE_FROM_CLIENT | Type.RECV_CLOSE_ON_SERVER -> ()
  | Type.SEND_MESSAGE ->
    let it = data |-> Data.send_message in
    Byte_buffer.destroy (it |->* Data.Send_message.send_message)
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
      ignore @@ Option.map (Slice.unref <@ deref) (it |->* status_details))
  | Type.RECV_INITIAL_METADATA ->
    let it = data |-> Data.recv_initial_metadata in
    let md' = it |->* Data.Recv_initial_metadata.recv_initial_metadata in
    let md = md' |->* Metadata.metadata in
    let len = md' |->* Metadata.count |> Unsigned.Size_t.to_int in
    Metadata.destroy_entries md len;
    Metadata.destroy md'
  | Type.RECV_MESSAGE ->
    let it = data |-> Data.recv_message in
    Byte_buffer.destroy !@(it |->* Data.Recv_message.recv_message)
  | Type.RECV_STATUS_ON_CLIENT ->
    let it = data |-> Data.recv_status_on_client in
    Data.Recv_status_on_client.(
      Metadata.(
        let md = it |->* trailing_metadata in
        let md' = md |->* metadata in
        let len = md |->* count |> Unsigned.Size_t.to_int in
        destroy_entries md' len;
        destroy md);
      ignore @@ Option.map free (it |->* error_string);
      ignore @@ Option.map (Slice.unref <@ deref) (it |->* status_details))
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
        Option.map (Ctypes.addr <@ Slice.from_string ~copy:true) st.Status.details
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
    | `Recv_initial_metadata ref ->
      let it = data |-> Data.recv_initial_metadata in
      it |-> Data.Recv_initial_metadata.recv_initial_metadata <-@ ref.metadata
    | `Recv_message ref ->
      let it = data |-> Data.recv_message in
      it |-> Data.Recv_message.recv_message <-@ ref.message
    | `Recv_status_on_client ref ->
      let it = data |-> Data.recv_status_on_client in
      Data.Recv_status_on_client.(
        it |-> trailing_metadata <-@ ref.metadata;
        it |-> status <-@ ref.status;
        it |-> status_details <-@ Some ref.details;
        it |-> error_string <-@ Some ref.error_message)
    | `Recv_close_on_server ref ->
      let it = data |-> Data.recv_close_on_server in
      it |-> Data.Recv_close_on_server.cancelled <-@ ref.cancelled
  in
  t
;;

let make_ops = List.map make
