open Base
module Sim = Event_driven_sim.Simulator
module Signal = Sim.Signal
module Process = Sim.Process
module Async = Sim.Async
open Sim

let pf = Stdio.printf

let update_status link nxt_sts =
  let open Link.Link_signal.Link_value in
  let delay = get_delay !!link.status in
  let lv = { !!link with status = nxt_sts; update_time = Async.current_time () } in
  (link <--- lv) ~delay
;;

let send_proc uls send_time () =
  let open Link.Link_signal.Link_value in
  let handle_send ul =
    match !!ul.status with
    | Ready -> update_status ul (Sending send_time)
    | Received -> update_status ul Complete
    | _ -> ()
  in
  Array.iter uls ~f:handle_send
;;

let recv_proc dls () =
  let open Link.Link_signal.Link_value in
  let handle_recv dl =
    match !!dl.status with
    | Sending delay ->
      let lv = { !!dl with status = Received; update_time = Async.current_time () } in
      (dl <--- lv) ~delay
    | _ -> ()
  in
  Array.iter dls ~f:handle_recv
;;

let make_recv_procs n dst_mat link_mat =
  let trigger_recv xpu_id =
    let dls = Link.downlinks_for_switch dst_mat link_mat xpu_id in
    let dl_ids = Array.map dls ~f:Signal.id in
    Process.create (Array.to_list dl_ids) (recv_proc dls)
  in
  Array.init n ~f:trigger_recv
;;

let make_procs_list xs = List.map xs ~f:Array.to_list |> List.concat

let all2all_send_receive n payload dst_mat link_mat =
  (* Send to all uplinks *)
  let trigger_send xpu_id =
    let payload = payload / n in
    let send_time = payload / 1 in
    let uls = link_mat.(xpu_id) in
    let ul_ids = Array.map uls ~f:Signal.id in
    Process.create (Array.to_list ul_ids) (send_proc uls send_time)
  in
  let send_procs = Array.init n ~f:trigger_send in
  let recv_procs = make_recv_procs n dst_mat link_mat in
  make_procs_list [ send_procs; recv_procs ]
;;

let ring_send_receive _n _payload dst_mat link_mat =
  let open! Sim in
  let open! Link.Link_signal.Link_value in
  (* 1. get terminal uplinks which is equal to switch downlinks.*)
  (* 2. for each switch_id, collect downlinks
     3. on the downlinks for switch, initiate a send
     4. on the uplink for switch, initiate accept
     5. on the uplink for switch-to-switch, initiate a send
     6. on the downlink for switch-to-switch, initiate an accept
     7. on the uplink for switch-to-terminal, initiate a send
     8. on the downlink for switch-to-terminal, initiate accept
     9. overall 3 sends and 3 accepts per stage
  *)
  (* send and receive in one proc?
     one proc for each signal
     we have signal for uplink and downlink
     so, total signals should 2 * links
  *)
  (* dst_sw -> switch_count x out_degree_switches + switch_count x terminal *)
  (* ccl process has access to signals. Responsible for changing some states on the signals *)
  let run_stage dst_mat link_mat =
    let _dst_sw_sw = dst_mat.(0) in
    let _dst_sw_t = dst_mat.(1) in
    let _dst_t_sw = dst_mat.(2) in
    let _link_sw_sw = link_mat.(0) in
    let _link_sw_t = link_mat.(1) in
    let _link_t_sw = link_mat.(2) in
    pf "T count:%d\n" (Array.length _dst_sw_t)
  in
  (* let send_procs = *)
  (*   Array.map link_mat ~f:(fun uls -> *)
  (*     let payload = payload / n in *)
  (*     let send_time = payload / 1 in *)
  (*     (\* Create process *\) *)
  (*     (\* Local counter to update - maintained outside the process *\) *)
  (*     (\* Mark complete when counter is exhausted *\) *)
  (*     let ul_ids = Array.map uls ~f:Signal.id in *)
  (*     let counter = ref (n - 1) in *)
  (*     let send_n_times () = *)
  (*       let handle_send ul = *)
  (*         match !!ul.status with *)
  (*         | Ready -> *)
  (*           counter := !counter - 1; *)
  (*           update_status ul (Sending send_time) *)
  (*         | Received when Int.(!counter = 0) -> update_status ul Complete *)
  (*         | Received -> *)
  (*           counter := !counter - 1; *)
  (*           update_status ul (Sending send_time) *)
  (*         | _ -> () *)
  (*       in *)
  (*       Array.iter uls ~f:handle_send *)
  (*     in *)
  (*     Process.create (Array.to_list ul_ids) send_n_times) *)
  (* in *)
  (* let recv_procs = make_recv_procs n dst_mat link_mat in *)
  (* make_procs_list [ send_procs; recv_procs ] *)
  run_stage dst_mat link_mat;
  []
;;

let reduce_scatter conn payload dst_mat link_mat =
  match conn with
  (* | Conn_type.All2All _ -> all2all_send_receive n payload dst_mat link_mat *)
  | Conn_type.Ring n -> ring_send_receive n payload dst_mat link_mat
  | _ -> []
;;
