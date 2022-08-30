type 'a ok = 'a * Metadata.bwd

type 'st fail' = 'st * string option * Metadata.bwd
  constraint 'st = [< Status.Code.fail_bwd ]

and fail = Status.Code.fail_bwd fail'

type 'a comp = ('a, fail) result
and 'res res = 'res ok comp

type ('bwd, 'fwd) handler = Context.t -> Metadata.bwd -> Call.t -> 'bwd -> 'fwd res Lwt.t

(** string as HTTP request/response body *)
type base = string

type t =
  | Unary :
      { handler : ('bwd, 'fwd) handler
      ; marshal : 'fwd -> base
      ; unmarshal : base -> ('bwd, [ `FAILED_PRECONDITION ] * string option) Result.t
      }
      -> t

let make ~marshal ~unmarshal ~handler = Unary { handler; marshal; unmarshal }
