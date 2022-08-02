open Import

let init =
  let it = lazy (u @@ F.init ()) in
  fun () -> Lazy.force it
;;

let shutdown =
  let it = lazy (u @@ F.shutdown ()) in
  fun () -> Lazy.force it
;;

let is_initialized () = u @@ F.is_initialized ()
