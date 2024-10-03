open Base
module Sim = Event_driven_sim.Simulator
module Signal = Sim.Signal
module Process = Sim.Process
module Async = Sim.Async
module Debug = Sim.Debug
module Link = Toposim.Link
module Ccl = Toposim.Ccl
module Conn_type = Toposim.Conn_type
module LT = Toposim.Link_type
open Link

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

let make_links dst_mat =
  Array.map dst_mat ~f:(fun dsts ->
    Array.init (Array.length dsts) ~f:(fun _ ->
      Signal.create (module Link_signal.Link_value)))
;;

let uplink_procs link_mat src_ids src_id dsts =
  let open Sim in
  let open Link_signal.Link_value in
  let uls = link_mat.(src_id) in
  let ul_ids = Array.map uls ~f:Signal.id in
  let handle_ul () =
    let establish_conn i ul =
      match !!ul.status with
      | Undefined ->
        let delay = get_delay Undefined in
        (* pf "i:%d|src:%d|dst:%d\n" src_id src_ids.(src_id) dsts.(i); *)
        let lv = make_t src_ids.(src_id) dsts.(i) Connecting (Async.current_time ()) in
        (ul <--- lv) ~delay
      | _ -> ()
    in
    Array.iteri uls ~f:establish_conn
  in
  (* switch creates a separate process for uplinks and downlinks *)
  Process.create (Array.to_list ul_ids) handle_ul
;;

let downlink_procs dls =
  let open Sim in
  let open Link_signal.Link_value in
  (* pf "DL len:%d\n" (Array.length dls); *)
  let dl_ids = Array.map dls ~f:Signal.id in
  let handle_dls () =
    let handle_dl dl =
      match !!dl.status with
      | Connecting ->
        let delay = get_delay !!dl.status in
        let lv = { !!dl with status = Ready; update_time = Async.current_time () } in
        (dl <--- lv) ~delay
      | _ -> ()
    in
    Array.iter dls ~f:handle_dl
  in
  Process.create (Array.to_list dl_ids) handle_dls
;;

let make_procs link_type dst_mat link_mat =
  let downlinks link_type =
    match link_type with
    | LT.SwitchToSwitch ->
      Array.init (Array.length dst_mat) ~f:(Link.downlinks_for_switch dst_mat link_mat)
    | LT.SwitchToTerminal ->
      Array.init
        (Array.length dst_mat)
        ~f:(Link.downlinks_from_terminals dst_mat link_mat)
    | LT.TerminalToSwitch ->
      (* Every terminal has a downlink from exactly one switch
         This may change in the future but for now
      *)
      _shape_str link_mat;
      [||]
  in
  let get_src_id link_type switch_count src_id =
    match link_type with
    | LT.SwitchToSwitch | LT.SwitchToTerminal -> src_id
    | LT.TerminalToSwitch ->
      let result = switch_count + src_id in
      result
  in
  let switch_count = Array.length dst_mat in
  let src_ids = Array.init switch_count ~f:(get_src_id link_type switch_count) in
  let f = uplink_procs link_mat src_ids in
  let uprocs = Array.mapi dst_mat ~f in
  let dls = downlinks link_type in
  let dprocs = Array.map dls ~f:downlink_procs in
  Array.concat [ uprocs; dprocs ]
;;

let _find_neighbors n conn xpu_id =
  match conn with
  | Conn_type.All2All _xs ->
    let xs = Array.init n ~f:(fun id -> if Int.(id <> xpu_id) then id else -1) in
    Array.filter xs ~f:(fun x -> Int.(x <> -1))
  | Conn_type.Ring _xs -> [| (xpu_id + 1) % n |]
  | _ -> [||]
;;

let _transpose mat =
  let dimy = Array.length mat in
  assert (dimy > 0);
  let dimx = Array.length mat.(0) in
  let tmat = Array.make_matrix ~dimx ~dimy 0 in
  Array.iteri mat ~f:(fun i row -> Array.iteri row ~f:(fun j e -> tmat.(j).(i) <- e));
  tmat
;;

let terminal_id switch_count term_count sw_id term_idx =
  switch_count + (sw_id * term_count) + term_idx
;;

let routing_table = function
  | Conn_type.HyperX (_, _, _) as _hx -> [||]
  | Conn_type.Ring n as r ->
    let t = Conn_type.terminal_count r in
    let d = Conn_type.degree r in
    let p = Conn_type.switch_count r in
    (* two signals per link. *)
    (* take one signal for every link visible from the switch  *)
    let result =
      Array.init p ~f:(fun sw_id ->
        let terminals = Array.init t ~f:(terminal_id sw_id p t) in
        let switches = [| (n + sw_id - 1) % n; (sw_id + 1) % n |] in
        assert (Array.length switches + Array.length terminals = d);
        switches, terminals)
    in
    let t_to_sw = Array.init (p * t) ~f:(fun t_id -> [| t_id / t |]) in
    let sw_to_sw, sw_to_t = Array.unzip result in
    [| sw_to_sw; sw_to_t; t_to_sw |]
  | _ -> [||]
;;

(* Divide total nodes into nodes per switch *)
(* All nodes connected to the switch are in all2all *)
(* let conn = Conn_type.make_hyperx ~t:1 ~m:[| 4 |] ~k:1 *)
let conn = Conn_type.Ring 4

let add_debug link_type links =
  let link_name =
    match link_type with
    | LT.SwitchToSwitch -> "sw_to_sw_link"
    | LT.SwitchToTerminal -> "sw_to_t_link"
    | LT.TerminalToSwitch -> "t_to_sw_link"
  in
  let mat =
    Array.mapi links ~f:(fun i ->
      Array.map ~f:(Debug.print_signal (link_name ^ Int.to_string i)))
  in
  flatten_mat mat
;;

let () =
  let all_dst_mats = routing_table conn in
  let all_link_mats = Array.map ~f:make_links all_dst_mats in
  let all_link_types =
    [| LT.SwitchToSwitch; LT.SwitchToTerminal; LT.TerminalToSwitch |]
  in
  let procs =
    Array.init (Array.length all_link_types) ~f:(fun i ->
      make_procs all_link_types.(i) all_dst_mats.(i) all_link_mats.(i))
  in
  let dbgs =
    Array.init (Array.length all_link_types) ~f:(fun i ->
      add_debug all_link_types.(i) all_link_mats.(i))
  in
  let dbgs = Array.to_list dbgs |> Array.concat |> Array.to_list in
  let procs = Array.to_list procs |> Array.concat |> Array.to_list in
  (* let comms = Ccl.reduce_scatter conn (4 * 100) dst_sw_sw all_link_mats.(0) in *)
  (* Link.build_trace "reduce_scatter" link_mat; *)
  let xpus = procs @ dbgs in
  let xpusim = Sim.create xpus in
  Sim.run xpusim ~time_limit:1500
;;
