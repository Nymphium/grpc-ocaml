include Grpc_basic.Log

let default = Logs.Src.create "grpc.server"
let src = default
let header = "grpc.server"

module Tag = struct
  include Tag

  let target = Tag.str ~name:"target"

  let request =
    let def = Logs.Tag.def "request" Grpc_basic.Headers.pp in
    Logs.Tag.add def
  ;;
end

module Export = struct
  let src = src
end
