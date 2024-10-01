open Base

type t =
  | All2All of int
  | Ring of int
  | HyperX of int * int array * int
[@@deriving sexp_of, compare, equal]

let make_hyperx ~t ~m ~k = HyperX (t, m, k)

let switch_count = function
  | HyperX (_, m, _) -> Array.fold m ~init:0 ~f:Int.( + )
  | Ring n -> n
  | _ -> 0
;;

let node_count = function
  | HyperX (t, _, _) as hx -> switch_count hx * t
  | Ring _ as r -> switch_count r * 1
  | _ -> 0
;;

let degree = function
  | HyperX (t, _, _) as hx -> t + switch_count hx
  | Ring _ -> 1 + 2 (* terminal + switch_count *)
  | _ -> 0
;;

let link_count = function
  | HyperX _ as hx -> degree hx * switch_count hx / 2
  | Ring n as r ->
    let terminal_links = 1 * n in
    let switch_links = 2 * switch_count r / 2 in
    terminal_links + switch_links
  | _ -> 0
;;

let terminal_count = function
  | HyperX (t, _, _) -> t
  | Ring _ -> 1
  | _ -> 0
;;
