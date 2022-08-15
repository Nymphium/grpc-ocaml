type 'a ok = 'a * Metadata.bwd

type 'st fail = 'st * string option * Metadata.bwd
  constraint 'st = [< Status.Code.fail_bwd ]

type 'res res = ('res ok, Status.Code.fail_bwd fail) result
type ('bwd, 'fwd) handler = Context.t -> Metadata.bwd -> 'bwd -> 'fwd res Lwt.t

(** string as HTTP request/response body *)
type base = string

type t =
  | Unary :
      { handler : ('bwd, 'fwd) handler
      ; marshall : 'fwd -> base
      ; unmarshall : base -> ('bwd, [ `FAILED_PRECONDITION ] * string option) Result.t
      }
      -> t

let make ~marshall ~unmarshall ~handler = Unary { handler; marshall; unmarshall }
