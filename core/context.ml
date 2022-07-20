include Hmap.Make (struct
  (** key * its printer *)
  type 'a t = string * ('a -> string)
end)

let create_key ?printer key =
  match printer with
  | Some printer -> Key.create (key, printer)
  | None -> Key.create (key, fun _ -> key)
;;

let host : string key = create_key "host"
let get_host = get host
let target : string key = create_key "target"
let get_target = get target
