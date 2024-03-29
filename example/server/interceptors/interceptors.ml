let counter = ref 0

let m =
  Grpc_server.Interceptors.(
    empty
    |> add (fun ctx headers _raw_data ->
           incr counter;
           let request_id =
             List.assoc_opt "request-id" headers
             |> (function
                  | None -> List.assoc_opt "x-request-id" headers
                  | Some _ as some -> some)
             |> Option.value
                  ~default:(Random.int 50000 |> Printf.sprintf "request-id-is-%d")
           in
           Grpc_server.Context.(
             add Context.request_id request_id ctx |> add Context.counter !counter)))
;;
