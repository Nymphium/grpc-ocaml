ocaml-grpc
===

a very-limited gRPC server/client implementation in OCaml

# demo
```shell
$ direnv allow
$ dune exec example/server.exe &
server.exe: [DEBUG] run gRPC echo server on localhost:20000 ...
$ dune exec example/client.exe
client.exe: [grpc.client] connect
client.exe: [grpc.client] send request
server.exe: [grpc.client] get request /grpc_test.Echo/Greet
server.exe: [DEBUG] request-id: request-id-is-9344
server.exe: [grpc.client] send response
client.exe: [grpc.client] receive response
client.exe: [DEBUG] response: hello
client.exe: [grpc.client] disconnect
```
