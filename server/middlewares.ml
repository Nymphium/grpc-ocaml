type t = Context.t -> H2.Reqd.t -> Context.t

let empty : t = Fun.const

let add : t -> t -> t =
 fun newm m ctx reqd ->
  let ctx' = m ctx reqd in
  newm ctx' reqd
;;
