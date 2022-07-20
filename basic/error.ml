open Grpc_core

type t = Status.Code.fail_bwd

let show = Status.Code.show_fail_bwd
let to_enum = Status.Code.fail_bwd_to_enum
let of_enum = Status.Code.fail_bwd_of_enum
