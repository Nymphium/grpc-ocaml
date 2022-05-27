type t = Grpc_basic.Error.t * string

let make ~status ~msg : t = status, msg
