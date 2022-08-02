open Import

open struct
  module M = T.Call
end

type hoge_values

let hoge : hoge_values structure typ = Ctypes.structure "hoge_values"
let hoge_f1 = Ctypes.field hoge "f1" Ctypes.string
let () = Ctypes.seal hoge
let hoge_new () = Ctypes.make hoge

(** wrapped call data *)
type t =
  { call : M.t
  ; cq : T.Completion.Queue.t
  ; flags : T.Flags.Write.t
  }

type res_res =
  { status : T.Status_code.t
  ; details : string option
  ; metadata : (string * string) list
  ; error : string option
  }

type res_raw =
  { recv_initial_metadata : (string * string) list
  ; recv_message : string option
  ; recv_status_on_client : res_res
  }

let run_batch t ops =
  let size_ops = List.length ops |> Unsigned.Size_t.of_int in
  let tag = Ctypes.null in
  let%lwt ops =
    CArray.(start <@ of_list T.Op.t)
    <@ List.map (fun op ->
           op @. T.Op.flags <-@ t.flags;
           op)
    =|< Op.make_ops ops
  in
  (* Ctypes.format T.Op.t Format.std_formatter !@ops; *)
  (* flush stdout; *)
  let*? err = F.Call.start_batch t.call ops size_ops tag __reserved__ in
  let%lwt () =
    if err <> M.Error.OK
    then Lwt.fail_with @@ Printf.sprintf "prepare call error(%s)" @@ M.Error.show err
    else Lwt.return_unit
  in
  let%lwt inf = Timespec.(inf_future Clock_type.realtime) in
  let%lwt ev = Completion_queue.pluck t.cq inf tag in
  if ev @.* T.Event.success < 1
  then (
    let st = ev @.* T.Event.typ in
    Lwt.fail_with @@ Printf.sprintf "run_batch failed(%s)" @@ T.Completion.Type.show st)
  else Lwt.return (tag, ops)
;;

let request ?(metadata = []) ?message t =
  let ops =
    let it =
      [ `Send_initial_metadata metadata
      ; `Send_close_from_client
      ; `Recv_message
      ; `Recv_initial_metadata
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
  let%lwt _t, o = run_batch t ops in
  let recv_initial_metadata = Lwt_mvar.create_empty () in
  let recv_message = Lwt_mvar.create_empty () in
  let recv_status_on_client = Lwt_mvar.create_empty () in
  let o =
    let size = List.length ops in
    CArray.(to_list @@ from_ptr o size)
  in
  let%lwt _ =
    Lwt.all
    @@ Fun.flip List.map o
    @@ fun op ->
    let data = op @.* T.Op.data in
    match op @.* T.Op.op with
    | T.Op.Type.RECV_INITIAL_METADATA ->
      let it = data @.* T.Op.Data.recv_initial_metadata in
      let md = deref @@ it @.* T.Op.Data.Recv_initial_metadata.recv_initial_metadata in
      let%lwt md = Metadata.to_fwd md in
      Lwt_mvar.put recv_initial_metadata md
    | T.Op.Type.RECV_MESSAGE ->
      let it = data @.* T.Op.Data.recv_message in
      let msg_ptr = it @.* T.Op.Data.Recv_message.recv_message in
      let%lwt msg = Byte_buffer.to_string msg_ptr in
      Lwt_mvar.put recv_message msg
    | T.Op.Type.RECV_STATUS_ON_CLIENT ->
      let it = data @.* T.Op.Data.recv_status_on_client in
      let%lwt md =
        let it = deref @@ it @.* T.Op.Data.Recv_status_on_client.trailing_metadata in
        Metadata.to_fwd it
      in
      let status = deref @@ it @.* T.Op.Data.Recv_status_on_client.status in
      let%lwt details =
        let it = it @.* T.Op.Data.Recv_status_on_client.status_details in
        match it with
        | Some slice ->
          let%lwt s' = Slice.to_ocaml_string !@slice in
          Lwt.return (Some s')
        | None -> Lwt.return None
      in
      let error =
        match it @.* T.Op.Data.Recv_status_on_client.error_string with
        | Some s -> Some (deref s)
        | None -> None
      in
      Lwt_mvar.put recv_status_on_client { status; error; metadata = md; details }
    | _ -> Lwt.return_unit
  in
  let%lwt recv_initial_metadata = Lwt_mvar.take recv_initial_metadata in
  let%lwt recv_message = Lwt_mvar.take recv_message in
  let%lwt recv_status_on_client = Lwt_mvar.take recv_status_on_client in
  Lwt.return { recv_initial_metadata; recv_message; recv_status_on_client }
;;
