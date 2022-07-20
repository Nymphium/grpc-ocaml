open Core
open Ctypes

module Make (F : Ctypes.TYPE) = struct
  open F

  let enum_handler name idx = failwithf "unexpected enum value in %s: %Ld" name idx ()
  let enum' name enumr = enum ~typedef:true name ~unexpected:(enum_handler name) enumr
end

let void_function = static_funptr @@ void @-> returning void
