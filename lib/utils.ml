open Base

let pf = Stdio.printf
let _shape_str mat = pf "%d,%d\n" (Array.length mat) (Array.length mat.(0))

let flatten_mat mat =
  let dimx = Array.length mat in
  let dimy = Array.length mat.(0) in
  Array.init (dimx * dimy) ~f:(fun i ->
    let row_id = i / dimy in
    let col_id = i - (row_id * dimy) in
    (* pf "flatten:%d|%d\n" row_id col_id; *)
    mat.(row_id).(col_id))
;;

let transpose mat =
  let dimy = Array.length mat in
  assert (dimy > 0);
  let dimx = Array.length mat.(0) in
  let tmat = Array.make_matrix ~dimx ~dimy mat.(0).(0) in
  Array.iteri mat ~f:(fun i row -> Array.iteri row ~f:(fun j e -> tmat.(j).(i) <- e));
  tmat
;;
