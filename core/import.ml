module T = Grpc_stub.Type
module F = Grpc_stub.Functions
include Fun

let deref = Ctypes.( !@ )
let[@inline] ( |@.> ) acc f = f @@ deref acc
let[@inline] ( let<* ) acc f = acc |@.> f
let[@inline] ( <@ ) g f x = g (f x)
let[@inline] ( @.* ) t f = Ctypes.getf t f
let[@inline] ( |->* ) t f = deref Ctypes.(t |-> f)

let[@inline] malloc ?finalise t =
  let ptr =
    Ctypes.from_voidp t @@ F.Alloc.malloc @@ Unsigned.Size_t.of_int @@ Ctypes.sizeof t
  in
  let () = Option.iter (fun finalise -> Gc.finalise finalise ptr) finalise in
  ptr
;;

let[@inline] free t = F.Alloc.free @@ Ctypes.to_voidp t

(** [NULL] *)
let __reserved__ = Ctypes.null

include Lwt.Infix
include Ctypes

module Inspect = struct
  module type Structure = sig
    type t

    val t : t Ctypes.structure Ctypes.typ
  end

  let make_structure
      (type a)
      (m : (module Structure with type t = a))
      (t : a Ctypes.structure)
    =
    let module M = (val m) in
    let buf = Buffer.create 512 in
    let buf_fmt = Format.formatter_of_buffer buf in
    Ctypes.format M.t buf_fmt t;
    Buffer.contents buf
  ;;

  module type M = sig
    type t

    val t : t Ctypes.typ
  end

  let make (type a) (m : (module M with type t = a)) (t : a) =
    let module M = (val m) in
    let buf = Buffer.create 512 in
    let buf_fmt = Format.formatter_of_buffer buf in
    Ctypes.format M.t buf_fmt t;
    Buffer.contents buf
  ;;
end

let allocate_string () = Ctypes.(allocate_n string ~count:1)
