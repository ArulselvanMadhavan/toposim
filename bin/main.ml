open Base
module Sim = Event_driven_sim.Simulator
module Signal = Sim.Signal
module Process = Sim.Process
module Async = Sim.Async
module Debug = Sim.Debug
module Link = Toposim.Link
module Ccl = Toposim.Ccl
module Conn_type = Toposim.Conn_type
open Link

let _pf = Stdio.printf

let make_links dst_mat =
  Array.map dst_mat ~f:(fun dsts ->
    Array.init (Array.length dsts) ~f:(fun _ ->
      Signal.create (module Link_signal.Link_value)))
;;

(* (\* switch to terminal - uplink *\) *)
(* (\* terminal to switch - downlink *\) *)
(* let make_xpu link_mat dst_mat node_type xpu_id = *)
(*   let open Sim in *)
(*   let open Link_signal.Link_value in *)
(*   (\* Uplink Process *\) *)
(*   let uls = link_mat.(xpu_id) in *)
(*   let ul_ids = Array.map uls ~f:Signal.id in *)
(*   let dsts = dst_mat.(xpu_id) in *)
(*   let handle_ul () = *)
(*     (\* Establish uplinks *\) *)
(*     let establish_conn i ul = *)
(*       match !!ul.status with *)
(*       | Undefined -> *)
(*         let delay = get_delay Undefined in *)
(*         let lv = make_t xpu_id dsts.(i) Connecting (Async.current_time ()) in *)
(*         (ul <--- lv) ~delay *)
(*       | _ -> () *)
(*     in *)
(*     Array.iteri uls ~f:establish_conn *)
(*   in *)
(*   let ul_proc = Process.create (Array.to_list ul_ids) handle_ul in *)
(*   (\* Downlink process *\) *)
(*   let dls = *)
(*     match node_type with *)
(*     | `Switch -> Link.downlinks_for_xpu xpu_id dst_mat link_mat *)
(*     | `Terminal -> Link.downlinks_from_terminals xpu_id dst_mat link_mat *)
(*   in *)
(*   let dl_ids = Array.map dls ~f:Signal.id in *)
(*   let handle_dls () = *)
(*     let handle_dl dl = *)
(*       match !!dl.status with *)
(*       | Connecting -> *)
(*         let delay = get_delay !!dl.status in *)
(*         let lv = { !!dl with status = Ready; update_time = Async.current_time () } in *)
(*         (dl <--- lv) ~delay *)
(*       | _ -> () *)
(*     in *)
(*     Array.iter dls ~f:handle_dl *)
(*   in *)
(*   let dl_proc = Process.create (Array.to_list dl_ids) handle_dls in *)
(*   [ ul_proc; dl_proc ] *)
(* ;; *)

let uplink_procs link_mat src_id dsts =
  let open Sim in
  let open Link_signal.Link_value in
  let uls = link_mat.(src_id) in
  let ul_ids = Array.map uls ~f:Signal.id in
  let handle_ul () =
    let establish_conn i ul =
      match !!ul.status with
      | Undefined ->
        let delay = get_delay Undefined in
        let lv = make_t src_id dsts.(i) Connecting (Async.current_time ()) in
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
  match link_type with
  | `Switch_to_Switch ->
    let f = uplink_procs link_mat in
    let uprocs = Array.mapi dst_mat ~f in
    let dls =
      Array.init (Array.length dst_mat) ~f:(Link.downlinks_for_xpu dst_mat link_mat)
    in
    let dprocs = Array.map dls ~f:downlink_procs in
    Array.concat [ uprocs; dprocs ]
  | _ -> [||]
;;

let _find_neighbors n conn xpu_id =
  match conn with
  | Conn_type.All2All _xs ->
    let xs = Array.init n ~f:(fun id -> if Int.(id <> xpu_id) then id else -1) in
    Array.filter xs ~f:(fun x -> Int.(x <> -1))
  | Conn_type.Ring _xs -> [| (xpu_id + 1) % n |]
  | _ -> [||]
;;

let transpose mat =
  let dimy = Array.length mat in
  assert (dimy > 0);
  let dimx = Array.length mat.(0) in
  let tmat = Array.make_matrix ~dimx ~dimy 0 in
  Array.iteri mat ~f:(fun i row -> Array.iteri row ~f:(fun j e -> tmat.(j).(i) <- e));
  tmat
;;

let shape_str mat = pf "%d,%d\n" (Array.length mat) (Array.length mat.(0))

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
        let terminals = Array.init t ~f:(fun t_id -> p + (sw_id * t) + t_id) in
        let switches = [| (n + sw_id - 1) % n; (sw_id + 1) % n |] in
        assert (Array.length switches + Array.length terminals = d);
        switches, terminals)
    in
    result
  | _ -> [||]
;;

(* Divide total nodes into nodes per switch *)
(* All nodes connected to the switch are in all2all *)
(* let conn = Conn_type.make_hyperx ~t:1 ~m:[| 4 |] ~k:1 *)
let conn = Conn_type.Ring 4

let add_debug links node_type =
  let link_name =
    match node_type with
    | `Switch_to_Switch -> "sw_link"
    | `Terminal -> "terminal_link"
  in
  Array.mapi links ~f:(fun i ->
    Array.map ~f:(Debug.print_signal (link_name ^ Int.to_string i)))
;;

let () =
  let dst_mat = routing_table conn in
  let dst_sw_sw, dst_sw_t = Base.Array.unzip dst_mat in
  shape_str dst_sw_sw;
  shape_str dst_sw_t;
  let dst_t_sw = transpose dst_sw_t in
  shape_str dst_t_sw;
  let all_dst_mats = [| dst_sw_sw; dst_sw_t; dst_t_sw |] in
  let all_link_mats = Array.map ~f:make_links all_dst_mats in
  let procs_sw_sw = make_procs `Switch_to_Switch dst_sw_sw all_link_mats.(0) in
  let dbg_sw = add_debug all_link_mats.(0) `Switch_to_Switch in
  let dbgs =
    Array.fold (Array.concat [ dbg_sw ]) ~init:[] ~f:(Fn.flip List.cons)
    |> Array.concat
    |> Array.to_list
  in
  let switch_procs = Array.to_list procs_sw_sw in
  (* Array.fold (Array.concat [ procs_sw_sw ]) ~init:[] ~f:(Fn.flip List.cons) *)
  (* |> List.concat *)
  let comms = Ccl.reduce_scatter conn (4 * 100) dst_sw_sw all_link_mats.(0) in
  (* Link.build_trace "reduce_scatter" link_mat; *)
  let xpus = switch_procs @ dbgs @ comms in
  let xpusim = Sim.create xpus in
  Sim.run xpusim ~time_limit:1500
;;
