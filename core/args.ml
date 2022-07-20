open Import

open struct
  module M = T.Arg
end

module Value = struct
  type v =
    [ `String of string
    | `Int of int
    ]

  let make v =
    let t = Ctypes.make M.Value.t in
    let () =
      match v with
      | `String str -> t @. M.Value.string <-@ str
      | `Int int -> t @. M.Value.integer <-@ int
    in
    t
  ;;

  let inspect t =
    let<* string = t @. M.Value.string in
    let<* int = t @. M.Value.integer in
    Printf.sprintf {|{ string: "%s"; integer: %d; pointer: <ptr> }|} string int
  ;;
end

let inspect t =
  let typ =
    t @. M.typ
    |@.> function
    | M.Type.STRING -> "STRING"
    | M.Type.INTEGER -> "INTEGER"
    | M.Type.POINTER -> "POINTER"
  in
  let<* key = t @. M.key in
  let value = t @. M.value |@.> Value.inspect in
  Printf.sprintf {|{ type: %s; key: %s; value: %s }|} typ key value
;;

let make key value =
  let t = Ctypes.make M.t in
  let v = Value.make value in
  let typ =
    match value with
    | `String _ -> M.Type.STRING
    | `Int _ -> M.Type.INTEGER
  in
  let () =
    t @. M.typ <-@ typ;
    t @. M.key <-@ key;
    t @. M.value <-@ v
  in
  t
;;
