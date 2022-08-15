module Error = Error
module Headers = Headers
module Log = Log

type 'a ok = 'a Grpc_core.Protoiso.ok
type fail = Grpc_core.Protoiso.fail
type 'a comp = 'a Grpc_core.Protoiso.comp
type 'a res = 'a Grpc_core.Protoiso.res

let ok_raw ?(metadata = []) res = res, metadata

(** make success response  *)
let ok ?metadata res : 'a res = Result.ok @@ ok_raw ?metadata res

(** composition of [ok] and [Lwt.return] *)
let ok' ?metadata res = Lwt.return @@ ok ?metadata res

let fail_raw ?(status = `INTERNAL) ?(metadata = []) ?detail () : fail =
  status, detail, metadata
;;

(** make error response *)
let fail ?status ?metadata ?detail () : 'a comp =
  Result.error @@ fail_raw ?status ?metadata ?detail ()
;;

(** composition of [fail] and [Lwt.return] *)
let fail' ?status ?metadata ?detail () = Lwt.return @@ fail ?status ?metadata ?detail ()

(** let-operators support *)
module Syntax = struct
  let ok_raw = ok_raw
  let ok = ok
  let ok' = ok'
  let fail_raw = fail_raw
  let fail = fail
  let fail' = fail'

  (** bind *)
  let ( let* ) m k : 'a res Lwt.t = Lwt_result.bind m k

  (** map *)
  let ( let+ ) m k : 'a res Lwt.t = Lwt_result.map k m

  (** lift Result *)
  let ( let> ) m k : 'a res Lwt.t = Lwt_result.(bind (lift m) k)

  (** lift IO *)
  let ( let@ ) io k = Lwt_result.(bind (ok io) k)
end
