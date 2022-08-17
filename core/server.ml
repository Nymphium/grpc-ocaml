open Import

let default_service_handler_size = 5

open struct
  module M = struct
    include T.Server
    include F.Server

    let to_voidp = coerce t (ptr void)
    let is_null = is_null <@ to_voidp

    let assert_exists =
      let message = "server destroyed" in
      fun ?(message = message) svr -> if is_null svr then failwith message
    ;;

    (* let request_call srv call detail md cq cq' tag = *)
    (* Lwt_preemptive.detach request_call srv >|= fun f -> f call detail md cq cq' tag *)
    (* ;; *)
  end
end

module Credentials = struct
  type t = T.Server.Credentials.t

  let free = F.Server.Credentials.release
  let make_insecure = F.Server.Credentials.create_insecure
end

module Request_call_stack = struct
  type t

  let t : t structure typ = Ctypes.structure "grpc_ocaml_request_call_stack"
  let call = Ctypes.field t "cal" @@ T.Call.t
  let call_details = Ctypes.field t "call_details" @@ ptr T.Call_details.t
  let metadata = Ctypes.field t "metadata" @@ ptr T.Metadata.Array.t
  let () = seal t

  let destroy t =
    Call.Details.destroy (t |->* call_details);
    free t
  ;;

  let make_tag_pair ~call:c ~call_details:cd ~metadata:md =
    let t = malloc t in
    t |-> call <-@ c;
    t |-> call_details <-@ cd;
    t |-> metadata <-@ md;
    let tag = to_voidp t in
    t, tag
  ;;
end

module Rpc = struct
  type t =
    { host : string
    ; methd : string
    ; deadline : T.Timespec.t structure
    ; metadata : T.Metadata.Array.t structure ptr
    ; call : Call.t
    }

  let destroy { call; metadata; _ } =
    Call.destroy call;
    Metadata.destroy metadata
  ;;
end

(** Before handling RPC request, middlewares read context, headers and raw data, and inspect its context

{
empty |> add @@ fun ctx headers raw_data ->
  (* something *)
  let ctx' = Ctx.add key val ctx in
  ctx'
} *)
module Middlewares = struct
  type t = Context.t -> string option -> Metadata.bwd -> Context.t

  let empty : t = fun c _ _ -> c

  (** add middleware: read context, headers and raw data, and inspect context
   *)
  let add : t -> t -> t =
   fun newm m ctx data md ->
    let ctx' = m ctx data md in
    newm ctx' data md
 ;;
end

(** path * handlers *)
type handlers = (string, Protoiso.t) Hashtbl.t

type ('bwd, 'fwd) handler = ('bwd, 'fwd) Protoiso.handler

(** *external* representation of handler type *)
type handler_sort = [ `Unary ]

type state =
  [ `Running
  | `Not_started
  | `Stopped
  ]

type t =
  { server : M.t
  ; cq : T.Completion.Queue.t
  ; mutable state : state
  ; context : Context.t
  ; handlers : handlers
  ; middlewares : Middlewares.t
  ; lock : Lwt_mutex.t
  }

let is_running t = t.state = `Running

let assert_not_running =
  let message = "server already running" in
  fun ?(message = message) t -> if is_running t then failwith message
;;

let make args middlewares =
  Top.init ();
  let args =
    if List.length args > 0 then Some (addr @@ Channel.Args.make args) else None
  in
  let cq = Completion_queue.create_for_pluck () in
  let server = M.create args __reserved__ in
  let () = M.assert_exists ~message:"failed to create server" server in
  let () = M.register_completion_queue server cq __reserved__ in
  { server
  ; cq
  ; state = `Not_started
  ; handlers = Hashtbl.create default_service_handler_size
  ; context = Context.empty
  ; middlewares
  ; lock = Lwt_mutex.create ()
  }
;;

(** internal use *)
let request_call t =
  let call = Call.allocate () in
  let call_details = Call.Details.allocate () in
  let metadata = Metadata.allocate () in
  let stack, tag =
    Request_call_stack.make_tag_pair ~call:!@call ~call_details ~metadata
  in
  let cq = Completion_queue.create_for_pluck () in
  let () = M.assert_exists t.server in
  let err = M.request_call t.server call call_details metadata cq t.cq tag in
  let () =
    if err <> T.Call.Error.OK
    then failwith (Printf.sprintf "prepare call error(%s)" @@ T.Call.Error.show err)
  in
  let inf = Timespec.(inf_future Clock_type.realtime) in
  let ev = Completion_queue.pluck t.cq inf tag in
  let () =
    if not @@ Event.is_success ev
    then (
      let st = Event.type_of ev in
      failwith (Printf.sprintf "request_call failed(%s)" @@ T.Completion.Type.show st))
    (* else Lwt_result.return () *)
  in
  let deadline =
    Timespec.Clock_type.convert
      !@(call_details |-> T.Call_details.deadline)
      Timespec.Clock_type.realtime
  in
  let methd = Slice.to_string !@(call_details |-> T.Call_details.methd) in
  let host = Slice.to_string !@(call_details |-> T.Call_details.host) in
  let call = Call.wrap_raw ~flags:Unsigned.UInt32.zero ~cq ~call:!@call () in
  Request_call_stack.destroy stack;
  Rpc.{ methd; host; deadline; metadata; call }
;;

let shutdown t =
  if t.state <> `Running
  then Lwt.return_unit
  else (
    t.state <- `Stopped;
    Lwt_mutex.unlock t.lock;
    if M.is_null t.server
    then Lwt.return_unit
    else
      Lwt_mutex.with_lock t.lock
      @@ fun () ->
      M.shutdown_and_notify t.server t.cq null;
      M.cancel_all_calls t.server;
      (* wait all ops are cancelled *)
      let ev = Completion_queue.pluck t.cq Timespec.inf_future' null in
      let typ = Event.type_of ev in
      if typ <> T.Completion.Type.OP_COMPLETE
      then
        Log.message __FILE__ __LINE__ `Info
        @@ Printf.sprintf "bad shutdown_and_notify result: %s"
        @@ T.Completion.Type.show typ;
      M.destroy t.server;
      Lwt.return_unit)
;;

(** add address to listen. It can be called multiple times before start the server
   @param creds TODO: ccheck proper credentials *)
let add_host ~host ~port ?(creds = Credentials.make_insecure ()) t =
  let () =
    M.assert_exists t.server;
    assert_not_running t
  in
  let addr = Printf.sprintf "%s:%d" host port in
  let recvd_port = M.add_http2_port t.server addr creds in
  let _ = Credentials.free creds in
  if recvd_port = 0
  then failwith @@ Printf.sprintf "failed to add address to server %s" addr
;;

let add_handler t ~methd ~typ ~marshall ~unmarshall handler =
  let () =
    M.assert_exists t.server;
    assert_not_running t
  in
  match Hashtbl.find_opt t.handlers methd, typ with
  | None, `Unary ->
    Hashtbl.add t.handlers methd (Protoiso.make ~marshall ~unmarshall ~handler)
  | Some _, _ -> failwith @@ Printf.sprintf "handler already exists in %s" methd
;;

let exists_handler t ~methd = Hashtbl.mem t.handlers methd

(** RPC handling dispatcher:
   receive a rpc, find its corresponding handler, add `host` and `target` to the context and exec handler *)
let dispatch t Rpc.{ methd; host; metadata; call; deadline } =
  Lwt.protected
  @@ flip Lwt.catch (fun exn ->
         let bt = Printexc.get_backtrace () in
         let details = Printexc.to_string exn in
         let msg = String.concat "\n" [ details; bt ] in
         Log.message __FILE__ __LINE__ `Error msg;
         Lwt.return @@ Call.unary_response call ~code:`INTERNAL ~details None)
  @@ fun () ->
  match Hashtbl.find_opt t.handlers methd with
  | None -> Lwt.return @@ Call.unary_response ~code:`UNIMPLEMENTED call None
  | Some handler ->
    Log.message __FILE__ __LINE__ `Debug @@ Printf.sprintf "handler found in %s" methd;
    (* TODO: get metadata from request *)
    let req, md = Call.remote_read call false in
    let metadata = Metadata.to_bwd metadata in
    let context =
      t.middlewares t.context req md
      |> Context.(add target methd)
      |> Context.(add host) host
    in
    Call.with_timeout
      call
      deadline
      (match handler with
      | Unary { handler; marshall; unmarshall } ->
        let%lwt res =
          let open Lwt_result.Syntax in
          let* umreq =
            let req = Option.value ~default:"" req in
            Lwt_result.lift
            @@ Result.map_error (fun ((`FAILED_PRECONDITION as code), details) ->
                   code, details, metadata)
            @@ unmarshall req
          in
          handler context md umreq
        in
        (match res with
        | Ok (res, md) ->
          let res = marshall res in
          Lwt.return @@ Call.unary_response call ~md (Some res)
        | Error (code, details, md) ->
          let code = (code :> Status.Code.bwd) in
          Lwt.return @@ Call.unary_response call ~code ~md ?details None))
;;

(** start a server and run loop *)
let start t =
  let () =
    M.assert_exists t.server;
    assert_not_running t
  in
  M.start t.server;
  t.state <- `Running;
  let rec go () =
    if t.state <> `Running
    then (
      Lwt_mutex.unlock t.lock;
      Lwt.return_unit)
    else (
      let%lwt rio =
        Lwt_result.map_error Printexc.to_string
        @@ Lwt_result.catch
        @@ let%lwt rpc = Lwt_preemptive.detach request_call t in
           let () = Lwt.async (fun () -> dispatch t rpc >|= fun _ -> Rpc.destroy rpc) in
           Lwt.return ()
      in
      if t.state = `Running
      then Result.iter_error (Log.message __FILE__ __LINE__ `Error) rio;
      go ())
  in
  go ()
;;
