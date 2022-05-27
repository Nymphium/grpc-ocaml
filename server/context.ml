include Hmap.Make (struct
  type 'a t = string * ('a -> string)
end)
