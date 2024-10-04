open Base
module Sim = Event_driven_sim.Simulator
module Signal = Sim.Signal
module Process = Sim.Process
module Async = Sim.Async
open Sim
open Utils

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

let ring_send_receive n payload all_link_types _all_dst_mats all_link_mats =
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
  (* let run_stage () = *)
  (*   (\* let _dst_sw_sw = dst_mat.(0) in *\) *)
  (*   (\* let _dst_sw_t = dst_mat.(1) in *\) *)
  (*   (\* let _dst_t_sw = dst_mat.(2) in *\) *)
  (*   (\* let _link_sw_sw = link_mat.(0) in *\) *)
  (*   (\* let _link_sw_t = link_mat.(1) in *\) *)
  (*   (\* let _link_t_sw = link_mat.(2) in *\) *)
  (*   (\* pf "T count:%d\n" (Array.length _dst_sw_t) *\) *)
  (* in *)
  (* let send_procs = *)
  (*   Array.map link_mat ~f:(fun uls -> *)
  (*     let payload = payload / n in *)
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
  (* run_stage dst_mat link_mat; *)

  (* 1. Terminal to switch send - use uplinks *)
  let term_idx, _ =
    Array.findi_exn all_link_types ~f:(fun _i -> Link_type.equal TerminalToSwitch)
  in
  let payload = payload / n in
  (* let term_dst_mat = all_dst_mats.(term_idx) in *)
  let t_to_sw_lmat = all_link_mats.(term_idx) in
  (* let start_send_proc link_type _src_id uls = *)
  (*   let payload = payload / n in *)
  (*   let send_time = payload / 1 in *)
  (*   let ul_ids = Array.map uls ~f:Signal.id |> Array.to_list in *)
  (*   let send_n_times () = *)
  (*     let send ul = *)
  (*       match !!ul.status with *)
  (*       | Ready -> update_status ul (Sending send_time) *)
  (*       | _ -> () *)
  (*     in *)
  (*     Array.iter uls ~f:send *)
  (*   in *)
  (*   Process.create ul_ids send_n_times *)
  (* in *)
  (* Array.mapi term_link_mat ~f:start_send_proc *)
  let manage_term_links _src_id uls =
    let ul_ids = Array.map uls ~f:Signal.id |> Array.to_list in
    let has_data = ref 5 in
    let fill_buffer_proc () =
      let handle_fill ul =
        match !!ul.status with
        | Ready when !has_data > 0 -> update_status ul (NonEmptyBuffer payload)
        | NonEmptyBuffer payload ->
          let send_time = payload / 1 in
          update_status ul (Sending send_time)
        | Received ->
          Int.decr has_data;
          update_status ul ClearBuffer
        | ClearBuffer when !has_data > 0 -> update_status ul Ready
        | ClearBuffer -> update_status ul Complete
        | _ -> ()
      in
      Array.iter uls ~f:handle_fill
    in
    Process.create ul_ids fill_buffer_proc
  in
  let t_procs = Array.mapi t_to_sw_lmat ~f:manage_term_links in
  let sw_to_t_idx, _ =
    Array.findi_exn all_link_types ~f:(fun _i -> Link_type.equal SwitchToTerminal)
  in
  let sw_to_sw_idx, _ =
    Array.findi_exn all_link_types ~f:(fun _i -> Link_type.equal SwitchToSwitch)
  in
  let switch_procs t_to_sw_lmat _sw_to_t_lmat _sw_to_sw_lmat =
    let all_dls = flatten_mat t_to_sw_lmat in
    let dl_ids = Array.map all_dls ~f:Signal.id |> Array.to_list in
    let term_dls_proc () =
      let handle_term_dl dl =
        match !!dl.status with
        | Sending delay ->
          let lv = { !!dl with status = Received; update_time = Async.current_time () } in
          (dl <--- lv) ~delay
        | _ -> ()
      in
      Array.iter all_dls ~f:handle_term_dl
    in
    let tdls_proc = Process.create dl_ids term_dls_proc in
    [| tdls_proc |]
  in
  let sw_to_t_lmat = all_link_mats.(sw_to_t_idx) in
  let sw_to_sw_lmat = all_link_mats.(sw_to_sw_idx) in
  let t_to_s_procs = switch_procs t_to_sw_lmat sw_to_t_lmat sw_to_sw_lmat in
  Array.append t_procs t_to_s_procs
;;

let reduce_scatter conn payload all_link_types all_dst_mats all_link_mats =
  match conn with
  | Conn_type.Ring n ->
    ring_send_receive n payload all_link_types all_dst_mats all_link_mats
  | _ -> [||]
;;
