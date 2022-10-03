module Tag = struct
  let str ~name =
    let tag = Logs.Tag.def name (Fun.flip Format.fprintf "%s") in
    Logs.Tag.add tag
  ;;

  let int ~name =
    let tag = Logs.Tag.def name (Fun.flip Format.fprintf "%d") in
    Logs.Tag.add tag
  ;;

  let float ~name =
    let tag = Logs.Tag.def name (Fun.flip Format.fprintf "%f") in
    Logs.Tag.add tag
  ;;

  let empty = Logs.Tag.empty
end

(** set Logs logger to gRPC-Core log message callback with ["grpc.core"] for [src], ["grpc.core.file"] for file of the source, ["grpc.core.line"] for the line number of the file. *)
let set_core_callback () =
  let src = Logs.Src.create "grpc.core" in
  let file_tag = Tag.str ~name:"grpc.core.file" in
  let line_tag = Tag.int ~name:"grpc.core.line" in
  Grpc_core.Log.set_log_callback
  @@ fun ~file ~line ~log_level ~message ->
  let tags = Tag.empty |> file_tag file |> line_tag line in
  match log_level with
  | `Error -> Logs.err ~src @@ fun m -> m ~tags "%s" message
  | `Debug -> Logs.debug ~src @@ fun m -> m ~tags "%s" message
  | `Info -> Logs.info ~src @@ fun m -> m ~tags "%s" message
;;
