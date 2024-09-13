open Base
module Sim = Toposim.Simulator
module Signal = Sim.Signal
module Process = Sim.Process
module Async = Sim.Async
module Debug = Sim.Debug
open Link

let make_links dsts =
  Array.init (Array.length dsts) ~f:(fun _ ->
    Signal.create (module Link_signal.Link_value))
;;

let make_xpu link_mat dst_mat xpu_id =
  (* Each xpu can create multiple processes *)
  (* A process can create another process but we are not doing that now *)
  (* uplink processes - sensitive to all signals for which it's the src *)
  (* downlink processes - listen to changes from processes for which it's the dst *)
  let open Sim in
  let open Link_signal.Link_value in
  (* Uplink Process *)
  let uls = link_mat.(xpu_id) in
  let ul_ids = Array.map uls ~f:(Signal.id) in
  let dsts = dst_mat.(xpu_id) in
  let handle_ul () =
    (* Establish uplinks *)
    let establish_conn i ul =
      match !!ul.status with
      | Undefined ->
        let delay = get_delay Undefined in
        let lv = make_t xpu_id dsts.(i) Connecting (Async.current_time ()) in
        (ul <--- lv) ~delay;
      | _ -> ()
    in
    Array.iteri uls ~f:establish_conn
  in
  let ul_proc = Process.create (Array.to_list ul_ids) (handle_ul) in
  (* Downlink process  *)
  let dls = Link.downlinks_for_xpu xpu_id dst_mat link_mat in
  let dl_ids = Array.map dls ~f:(Signal.id) in
  let handle_dls () =
    let handle_dl dl =
    (match !!dl.status with
    | Connecting ->
      let delay = get_delay !!dl.status in
      let lv = {!!dl with status = Ready; update_time = Async.current_time ()} in
      (dl <--- lv) ~delay
    | _ -> ())
    in
    Array.iter dls ~f:handle_dl
  in
  let dl_proc = Process.create (Array.to_list dl_ids) (handle_dls) in
  [ul_proc; dl_proc]

;;

let find_neighbors n conn xpu_id =
  match conn with
  | `All2All ->
    let xs = Array.init n ~f:(fun id -> if Int.(id <> xpu_id) then id else -1) in
    Array.filter xs ~f:(fun x -> Int.(x <> -1))
;;

let num_xpus = 5

let () =
  let dst_mat = Array.init num_xpus ~f:(find_neighbors num_xpus `All2All) in
  let link_mat = Array.map dst_mat ~f:make_links in
  let xpus = Array.init num_xpus ~f:(make_xpu link_mat dst_mat) in
  let dbg_mat =
    Array.mapi link_mat ~f:(fun i ->
      Array.map ~f:(Debug.print_signal ("xpu_link" ^ Int.to_string i)))
  in
  let dbgs =
    Array.fold dbg_mat ~init:[] ~f:(Fn.flip List.cons) |> Array.concat |> Array.to_list
  in
  let xpus = Array.fold xpus ~init:[] ~f:(Fn.flip List.cons) |> List.concat in
  let comms = Ccl.reduce_scatter num_xpus `All2All (num_xpus * 100) dst_mat link_mat in
  let xpus = xpus @ comms @ dbgs in
  let xpusim = Sim.create xpus in
  Sim.run xpusim ~time_limit:200
;;
