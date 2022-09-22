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
$ dune exec example/server/main.exe &
main.exe: [DEBUG] run gRPC echo server on localhost:50051 ...
$ dune exec example/client/main.exe
main.exe: [grpc.server] get request on /grpc_test.Echo/Greet
main.exe: [DEBUG] x-foo: <empty>
main.exe: [DEBUG] request-id: request-id-is-9344
main.exe: [grpc.server] send response in 0.000029 sec
1
main.exe: [grpc.server] get request on /grpc_test.Echo/Greet
main.exe: [DEBUG] x-foo: <empty>
main.exe: [DEBUG] request-id: request-id-is-26685
main.exe: [grpc.server] send response in 0.000015 sec
2
main.exe: [grpc.server] get request on /grpc_test.Echo/Greet
main.exe: [DEBUG] x-foo: <empty>
main.exe: [DEBUG] request-id: request-id-is-30182
main.exe: [grpc.server] send response in 0.000015 sec
3
main.exe: [grpc.server] get request on /grpc_test.Echo/Greet
main.exe: [DEBUG] x-foo: <empty>
main.exe: [DEBUG] request-id: request-id-is-31641
main.exe: [grpc.server] send response in 0.000015 sec
4
main.exe: [grpc.server] get request on /grpc_test.Echo/Greet
main.exe: [DEBUG] x-foo: <empty>
main.exe: [DEBUG] request-id: request-id-is-30439
main.exe: [grpc.server] send response in 0.000027 sec
5
main.exe: [DEBUG] response: hello
```

# TODO
- remove all the `grep TODO`s
- implement [interop test](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md)
- credentials
- support other RPCs not implemented
- documentation
