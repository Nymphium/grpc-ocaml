open Grpc_server.Context

let request_id : string key = Key.create ("request-id", Fun.const "request-id")
let counter : int key = Key.create ("counter", Fun.const "counter")
