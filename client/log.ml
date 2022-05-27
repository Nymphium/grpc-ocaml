include Grpc_basic.Log

let default = Logs.Src.create "grpc.client"
let src = default
let header = "grpc.client"

module Export = struct
  let src = src
end
