module T = Stub.Type
module F = Stub.Functions

let u = Stub.unbox
let ( >|~= ) s f = Lwt.map f (u s)
let ( =~|< ) f s = s >|~= f
let ( >>~= ) s = Lwt.bind (u s)
let ( let*? ) stub f = stub >>~= f
let deref = Ctypes.( !@ )
let ( |@.> ) acc f = f @@ deref acc
let ( let<* ) acc f = acc |@.> f
let[@inline] ( <@ ) g f x = g (f x)
let[@inline] ( <~@ ) g f x = Lwt.map g (u @@ f x)

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
