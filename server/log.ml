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

  let upstream_headers =
    let def = Logs.Tag.def "grpc.server.upstream_headers" Grpc_basic.Headers.pp in
    Logs.Tag.add def
  ;;

  let downstream_headers =
    let def = Logs.Tag.def "grpc.server.downstream_headers" Grpc_basic.Headers.pp in
    Logs.Tag.add def
  ;;
end

module Export = struct
  let src = src
end
