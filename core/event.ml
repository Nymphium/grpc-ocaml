open Import

open struct
  module M = struct
    include T.Event
    include F.Event
  end
end

let allocate () = malloc M.t
let type_of t = t @.* M.typ
let is_success t = t @.* M.success <> 0
