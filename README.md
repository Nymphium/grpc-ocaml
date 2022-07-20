grpc-ocaml
===

[gRPC-Core](https://github.com/grpc/grpc) bindings for OCaml

- [x] Unary RPC
- [ ] Server streaming RPC
- [ ] Client streaming RPC
- [ ] Bidirectional streaming RPC
- [x] Context
- [x] Middlewares

# Package details
- `grpc.stub` ... just a C FFI using ctypes
- `grpc.core` ... a very thin wrapper for `grpc.stub`
- `grpc.basic` ... utilities (logger and header) for client and servers
- `grpc.client` ... client library using `gpc.core`
- `grpc.server` ... server library using `grpc.core`

# Demo
```shell
$ direnv allow
$ dune exec example/server.exe &
server.exe: [DEBUG] run gRPC echo server on localhost:20000 ...
$ dune exec example/client.exe
server.exe: [grpc.server] get request on /grpc_test.Echo/Greet
server.exe: [DEBUG] request-id: request-id-is-9344
server.exe: [DEBUG] send response: duration 30.279159545898438us
server.exe: [grpc.server] get request on /grpc_test.Echo/Greet
server.exe: [DEBUG] request-id: request-id-is-26685
server.exe: [DEBUG] send response: duration 149.25003051757812us
client.exe: [DEBUG] response: hello
```
