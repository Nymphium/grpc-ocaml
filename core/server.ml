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
  end
end

module Credentials = struct
  include T.Server.Credentials
  include F.Server.Credentials
end

module Request_call_stack = struct
  type t

  let t : t structure typ = Ctypes.structure "grpc_ocaml_request_call_stack"
  let call = Ctypes.field t "cal" @@ Call.t
  let call_details = Ctypes.field t "call_details" @@ ptr Call.Details.t
  let metadata = Ctypes.field t "metadata" @@ ptr Metadata.raw
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
    ; deadline : Timespec.t
    ; metadata : Metadata.raw structure ptr
    ; call : Call.t
    }

  let make ~host ~methd ~deadline ~metadata ~call =
    { host; methd; deadline; metadata; call }
  ;;

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
  type t = Context.t -> Metadata.bwd -> string option -> Context.t

  let empty : t = fun c _ _ -> c

  (** add middleware: read context, headers and raw data, and inspect context
   *)
  let add : t -> t -> t =
   fun newm m ctx md data ->
    let ctx' = m ctx md data in
    newm ctx' md data
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
  ; cq : Completion_queue.raw
  ; mutable state : state
  ; context : Context.t
  ; handlers : handlers
  ; middlewares : Middlewares.t
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
    if err <> Call.Error.OK
    then failwith (Printf.sprintf "prepare call error(%s)" @@ Call.Error.show err)
  in
  let inf = Timespec.(inf_future Clock_type.realtime) in
  let ev = Completion_queue.pluck t.cq inf tag in
  let () =
    if not @@ Event.is_success ev
    then (
      let st = Event.type_of ev in
      failwith (Printf.sprintf "request_call failed(%s)" @@ Completion_queue.Type.show st))
    (* else Lwt_result.return () *)
  in
  let deadline =
    Timespec.Clock_type.convert
      (call_details |->* Call.Details.deadline)
      Timespec.Clock_type.realtime
  in
  let methd = Slice.to_string (call_details |->* Call.Details.methd) in
  let host = Slice.to_string (call_details |->* Call.Details.host) in
  let call = Call.wrap_raw ~flags:Unsigned.UInt32.zero ~cq ~call:!@call () in
  Request_call_stack.destroy stack;
  Rpc.make ~methd ~host ~deadline ~metadata ~call
;;

let shutdown t =
  if t.state <> `Running
  then Lwt.return_unit
  else (
    t.state <- `Stopped;
    if not (M.is_null t.server) then M.shutdown_and_notify t.server t.cq null;
    M.cancel_all_calls t.server;
    (* wait all ops are cancelled *)
    let ev = Completion_queue.pluck t.cq Timespec.inf_future' null in
    let typ = Event.type_of ev in
    if typ <> Completion_queue.Type.OP_COMPLETE
    then
      Log.message __FILE__ __LINE__ `Info
      @@ Printf.sprintf "bad shutdown_and_notify result: %s"
      @@ Completion_queue.Type.show typ;
    M.destroy t.server;
    Lwt.return_unit)
;;

(** add address to listen. It can be called multiple times before start the server
   @param creds TODO: ccheck proper credentials *)
let add_host ~host ~port ?(creds = Credentials.create_insecure ()) t =
  let () =
    M.assert_exists t.server;
    assert_not_running t
  in
  let addr = Printf.sprintf "%s:%d" host port in
  let recvd_port = M.add_http2_port t.server addr creds in
  let () = Credentials.release creds in
  if recvd_port = 0
  then failwith @@ Printf.sprintf "failed to add address to server %s" addr
;;

let add_handler t ~methd ~typ ~marshal ~unmarshal handler =
  let () =
    M.assert_exists t.server;
    assert_not_running t
  in
  match Hashtbl.find_opt t.handlers methd, typ with
  | None, `Unary ->
    Hashtbl.add t.handlers methd (Protoiso.make ~marshal ~unmarshal ~handler)
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
    Log.message __FILE__ __LINE__ `Debug
    @@ Printf.sprintf "handler found { method=%s, host=%s }" methd host;
    let req = Call.remote_read call in
    let metadata = Metadata.to_bwd metadata in
    let context =
      t.middlewares t.context metadata req
      |> Context.(add target methd)
      |> Context.(add host) host
    in
    Call.with_timeout
      call
      deadline
      (match handler with
      | Unary { handler; marshal; unmarshal } ->
        let%lwt res =
          let open Lwt_result.Syntax in
          let* umreq =
            let req = Option.value ~default:"" req in
            Lwt_result.lift
            @@ Result.map_error (fun ((`FAILED_PRECONDITION as code), details) ->
                   code, details, metadata)
            @@ unmarshal req
          in
          handler context metadata umreq
        in
        (match res with
        | Ok (res, md) ->
          let res = marshal res in
          Lwt.return @@ Call.unary_response call ~tr:md (Some res)
        | Error (code, details, md) ->
          let code = (code :> Status.Code.bwd) in
          Lwt.return @@ Call.unary_response call ~code ~tr:md ?details None))
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
    then Lwt.return_unit
    else (
      let%lwt rio =
        Lwt_result.map_error Printexc.to_string
        @@ Lwt_result.catch
        @@ let%lwt rpc = Lwt_preemptive.detach request_call t in
           let () = Lwt.on_termination (dispatch t rpc) @@ fun () -> Rpc.destroy rpc in
           Lwt.return_unit
      in
      if t.state = `Running
      then Result.iter_error (Log.message __FILE__ __LINE__ `Error) rio;
      go ())
  in
  go ()
;;
