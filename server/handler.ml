open Core
open Fn

open struct
  module PB = Ocaml_protoc_plugin
end

type 'a res = ('a, Grpc_basic.Error.t * string option) Result.t Lwt.t
type ('req, 'res) handler = Context.t -> 'req -> 'res res
type sort = [ `Unary of (string, string) handler ]

(** The map from target to corresponding handler *)
module Map = struct
  include Map.Make (String)

  type nonrec t = sort t
end

(** Unary rpc handler *)
module Unary = struct
  let add
      : type req res.
        (module PB.Service.Rpc with type Request.t = req and type Response.t = res)
        -> (req, res) handler
        -> Map.t
        -> Map.t
    =
   fun rpc ->
    let module R = (val rpc) in
    let decode, encode = PB.Service.make_service_functions' rpc in
    let decode body =
      PB.Reader.create body
      |> decode
      |> Result.map_error ~f:(fun err ->
             `FAILED_PRECONDITION, Option.some @@ PB.Result.show_error err)
    in
    let encode = compose Ocaml_protoc_plugin.Writer.contents encode in
    let key = R.name' () in
    fun handler ->
      Map.add_exn
        ~key
        ~data:
          (`Unary
            (fun ctx body -> Lwt_result.(decode body |> lift >>= handler ctx >|= encode)))
 ;;
end
