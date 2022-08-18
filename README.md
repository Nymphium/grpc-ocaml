grpc-ocaml
===

[gRPC-Core](https://github.com/grpc/grpc) bindings for OCaml

- [x] Connection with insecure credentials
- [ ] Connection with credentials
- [x] Unary RPC
- [ ] Server streaming RPC
- [ ] Client streaming RPC
- [ ] Bidirectional streaming RPC
- [x] Context
- [x] Middlewares

# requirements
- libgrpc (>= 1.45)

# Package details
- `grpc.stub` ... just a C FFI using ctypes
- `grpc.core` ... wrappers for `grpc.stub` with lwt
- `grpc.basic` ... utilities (logger and header) for client and servers
- `grpc.client` ... client library using `gpc.core` with ocaml-protoc-plugin
- `grpc.server` ... server library using `grpc.core` with ocaml-protoc-plugin

# Demo
```shell
$ direnv allow
$ dune exec example/server.exe &
server.exe: [DEBUG] run gRPC echo server on localhost:20000 ...
$ dune exec example/client.exe
server.exe: [grpc.server] get request on /grpc_test.Echo/Greet
server.exe: [DEBUG] request-id: request-id-is-9344
server.exe: [grpc.server] send response in 0.000046 sec
server.exe: [grpc.server] get request on /grpc_test.Echo/Greet
server.exe: [DEBUG] request-id: request-id-is-26685
server.exe: [grpc.server] send response in 0.000110 sec
server.exe: [grpc.server] get request on /grpc_test.Echo/Greet
server.exe: [DEBUG] request-id: request-id-is-30182
server.exe: [grpc.server] send response in 0.000118 sec
server.exe: [grpc.server] get request on /grpc_test.Echo/Greet
server.exe: [DEBUG] request-id: request-id-is-31641
server.exe: [grpc.server] send response in 0.043040 sec
client.exe: [DEBUG] response: hello
```

# TODO
- remove all the `grep TODO`s
- implement [interop test](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md)
- credentials
- support other RPCs not implemented
- documentation
