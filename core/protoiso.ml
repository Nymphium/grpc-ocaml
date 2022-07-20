type 'res res =
  ('res * Metadata.bwd, Status.Code.fail_bwd * string option * Metadata.bwd) result

type ('bwd, 'fwd) handler = Context.t -> Metadata.bwd -> 'bwd -> 'fwd res Lwt.t
type base = string

type t =
  | Unary :
      { handler : ('bwd, 'fwd) handler
      ; marshall : 'fwd -> base
      ; unmarshall : base -> ('bwd, [ `FAILED_PRECONDITION ] * string option) Result.t
            (** success to convert or fail (:> Status.Code.bwd) *)
      }
      -> t

let make ~marshall ~unmarshall ~handler = Unary { handler; marshall; unmarshall }
