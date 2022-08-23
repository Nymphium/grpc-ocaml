open Import

module Code = struct
  include T.Status_code

  type raw = t

  (** @see https://github.com/googleapis/googleapis/blob/master/google/rpc/code.proto *)
  type fail_bwd =
    [ `CANCELLED [@value 1]
    | `UNKNOWN [@value 2]
    | `INVALID_ARGUMENT [@value 3]
    | `DEADLINE_EXCEEDED [@value 4]
    | `NOT_FOUND [@value 5]
    | `ALREADY_EXISTS [@value 6]
    | `PERMISSION_DENIED [@value 7]
    | `UNAUTHENTICATED [@value 16]
    | `RESOURCE_EXHAUSTED [@value 8]
    | `FAILED_PRECONDITION [@value 9]
    | `ABORTED [@value 10]
    | `OUT_OF_RANGE [@value 11]
    | `UNIMPLEMENTED [@value 12]
    | `INTERNAL [@value 13]
    | `UNAVAILABLE [@value 14]
    | `DATA_LOSS [@value 15]
    | `DO_NOT_USE [@value -1]
    ]
  [@@deriving show { with_path = false }, enum, eq]

  type bwd =
    [ `OK
    | fail_bwd
    ]

  let of_bwd : bwd -> t = function
    | `OK -> OK
    | `UNKNOWN -> UNKNOWN
    | `INTERNAL -> INTERNAL
    | `FAILED_PRECONDITION -> FAILED_PRECONDITION
    | `ABORTED -> ABORTED
    | `CANCELLED -> CANCELLED
    | `INVALID_ARGUMENT -> INVALID_ARGUMENT
    | `DEADLINE_EXCEEDED -> DEADLINE_EXCEEDED
    | `NOT_FOUND -> NOT_FOUND
    | `ALREADY_EXISTS -> ALREADY_EXISTS
    | `PERMISSION_DENIED -> PERMISSION_DENIED
    | `UNAUTHENTICATED -> UNAUTHENTICATED
    | `RESOURCE_EXHAUSTED -> RESOURCE_EXHAUSTED
    | `OUT_OF_RANGE -> OUT_OF_RANGE
    | `UNIMPLEMENTED -> UNIMPLEMENTED
    | `UNAVAILABLE -> UNAVAILABLE
    | `DATA_LOSS -> DATA_LOSS
    | `DO_NOT_USE -> DO_NOT_USE
  ;;

  let to_bwd : t -> bwd = function
    | OK -> `OK
    | UNKNOWN -> `UNKNOWN
    | INTERNAL -> `INTERNAL
    | FAILED_PRECONDITION -> `FAILED_PRECONDITION
    | ABORTED -> `ABORTED
    | CANCELLED -> `CANCELLED
    | INVALID_ARGUMENT -> `INVALID_ARGUMENT
    | DEADLINE_EXCEEDED -> `DEADLINE_EXCEEDED
    | NOT_FOUND -> `NOT_FOUND
    | ALREADY_EXISTS -> `ALREADY_EXISTS
    | PERMISSION_DENIED -> `PERMISSION_DENIED
    | UNAUTHENTICATED -> `UNAUTHENTICATED
    | RESOURCE_EXHAUSTED -> `RESOURCE_EXHAUSTED
    | OUT_OF_RANGE -> `OUT_OF_RANGE
    | UNIMPLEMENTED -> `UNIMPLEMENTED
    | UNAVAILABLE -> `UNAVAILABLE
    | DATA_LOSS -> `DATA_LOSS
    | DO_NOT_USE -> `DO_NOT_USE
  ;;
end

type t =
  { code : Code.t
  ; details : string option
  ; metadata : (string * string) list
  }

let allocate () = Ctypes.allocate_n Code.t ~count:1
