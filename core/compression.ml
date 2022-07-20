open Import

open struct
  module M = T.Compression
end

type algorithm =
  [ `None
  | `Deflate
  | `Gzip
  | `Count
  ]

type level =
  [ `None
  | `Low
  | `Med
  | `High
  | `Count
  ]

type t =
  { algorithm : algorithm
  ; level : level
  }
