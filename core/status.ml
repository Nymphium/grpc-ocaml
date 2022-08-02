open Import
module Code = T.Status_code

type t =
  { code : Code.t
  ; details : string option
  ; metadata : (string * string) list
  }
