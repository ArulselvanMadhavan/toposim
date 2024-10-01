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

let make_links dsts =
  Array.init (Array.length dsts) ~f:(fun _ ->
    Signal.create (module Link_signal.Link_value))
;;

let make_xpu link_mat dst_mat xpu_id =
  let open Sim in
  let open Link_signal.Link_value in
  (* Uplink Process *)
  let uls = link_mat.(xpu_id) in
  let ul_ids = Array.map uls ~f:Signal.id in
  let dsts = dst_mat.(xpu_id) in
  let handle_ul () =
    (* Establish uplinks *)
    let establish_conn i ul =
      match !!ul.status with
      | Undefined ->
        let delay = get_delay Undefined in
        let lv = make_t xpu_id dsts.(i) Connecting (Async.current_time ()) in
        (ul <--- lv) ~delay
      | _ -> ()
    in
    Array.iteri uls ~f:establish_conn
  in
  let ul_proc = Process.create (Array.to_list ul_ids) handle_ul in
  (* Downlink process *)
  let dls = Link.downlinks_for_xpu xpu_id dst_mat link_mat in
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
  let dl_proc = Process.create (Array.to_list dl_ids) handle_dls in
  [ ul_proc; dl_proc ]
;;

let pf = Stdio.printf

let _find_neighbors n conn xpu_id =
  match conn with
  | Conn_type.All2All _xs ->
    let xs = Array.init n ~f:(fun id -> if Int.(id <> xpu_id) then id else -1) in
    Array.filter xs ~f:(fun x -> Int.(x <> -1))
  | Conn_type.Ring _xs -> [| (xpu_id + 1) % n |]
  | _ -> [||]
;;

let routing_table = function
  | Conn_type.HyperX (_, _, _) as _hx -> [||]
  | Conn_type.Ring n as r ->
    let t = Conn_type.terminal_count r in
    let d = Conn_type.degree r in
    let p = Conn_type.switch_count r in
    let s = d - t in
    (* two signals per link. *)
    (* take one signal for every link visible from the switch  *)
    pf "S:%d|T:%d|D:%d\n" p t s;
    Array.init p ~f:(fun sw_id ->
      let terminals = Array.init t ~f:(fun t_id -> p + (sw_id * t) + t_id) in
      let switches = [| (n + sw_id - 1) % n; (sw_id + 1) % n |] in
      assert (Array.length switches + Array.length terminals = d);
      switches, terminals)
  | _ -> [||]
;;

(* Divide total nodes into nodes per switch *)
(* All nodes connected to the switch are in all2all *)
(* let conn = Conn_type.make_hyperx ~t:1 ~m:[| 4 |] ~k:1 *)
let conn = Conn_type.Ring 4

let () =
  let dst_mat = routing_table conn in
  let dst_sw, dst_t = Base.Array.unzip dst_mat in
  let link_sw = Array.map dst_sw ~f:make_links in
  let link_t = Array.map dst_t ~f:make_links in
  let switches = Array.init (Array.length dst_sw) ~f:(make_xpu link_sw dst_sw) in
  let terminals = Array.init (Array.length dst_t) ~f:(make_xpu link_t dst_t) in
  let dbg_sw =
    Array.mapi link_sw ~f:(fun i ->
      Array.map ~f:(Debug.print_signal ("sw_link" ^ Int.to_string i)))
  in
  let dbg_t =
    Array.mapi link_t ~f:(fun i ->
      Array.map ~f:(Debug.print_signal ("terminal_link" ^ Int.to_string i)))
  in
  let dbgs =
    Array.fold (Array.concat [ dbg_sw; dbg_t ]) ~init:[] ~f:(Fn.flip List.cons)
    |> Array.concat
    |> Array.to_list
  in
  let xpus =
    Array.fold (Array.concat [ switches; terminals ]) ~init:[] ~f:(Fn.flip List.cons)
    |> List.concat
  in
  (* let comms = Ccl.reduce_scatter num_xpus conn (num_xpus * 100) dst_mat link_mat in *)
  (* Link.build_trace "reduce_scatter" link_mat; *)
  let xpus = xpus @ dbgs in
  let xpusim = Sim.create xpus in
  Sim.run xpusim ~time_limit:1500
;;
