module Error = Error
module Utils = Utils
module Const = Const
module Headers = Headers
module Log = Log

type status =
  [ `OK
  | Error.t
  ]

let status_to_int = function
  | `OK -> 0
  | #Error.t as st -> Error.to_enum st
;;
