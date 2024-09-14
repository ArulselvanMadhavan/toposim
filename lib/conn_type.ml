type t =
  | All2All
  | Ring
[@@deriving sexp_of, compare, equal]
