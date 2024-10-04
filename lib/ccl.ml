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
  let payload = payload / n in
  let idx_of link_type =
    Array.findi_exn all_link_types ~f:(fun _i -> Link_type.equal link_type)
  in
  let t_to_sw_idx, _ = idx_of TerminalToSwitch in
  let sw_to_t_idx, _ = idx_of SwitchToTerminal in
  let sw_to_sw_idx, _ = idx_of SwitchToSwitch in
  let t_to_sw_lmat = all_link_mats.(t_to_sw_idx) in
  let sw_to_t_lmat = all_link_mats.(sw_to_t_idx) in
  let sw_to_sw_lmat = all_link_mats.(sw_to_sw_idx) in

  let term_procs _src_id uls =
    let ul_ids = Array.map uls ~f:Signal.id |> Array.to_list in
    let has_data = ref 1 in
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
  let switch_procs t_to_sw_lmat sw_to_t_lmat sw_to_sw_lmat =
    (* Handle dls *)
    let term_dls = flatten_mat t_to_sw_lmat in
    let sw_dls = flatten_mat sw_to_sw_lmat in
    let dls = Array.append term_dls sw_dls in
    let dls_ids = Array.map dls ~f:Signal.id |> Array.to_list in
    let dls_proc () =
      let handle_dl dl =
        match !!dl.status with
        | Sending payload ->
          let sw_id = !!dl.dst in
          let src = !!dl.src in
          let is_terminal = src >= n in
          let next_link = if is_terminal then sw_to_sw_lmat.(sw_id) else sw_to_t_lmat.(sw_id) in
          (* there will be multiple candidates to receive. topology selects the next link *)
          (* ASSUME: Ring always send to the right.
             ASSUME: There is only one terminal *)
          (* let next_link = sw_to_sw_lmat.(sw_id) |> Array.last in *)
          let next_link = Array.last next_link in
          update_status dl Received;
          update_status next_link (NonEmptyBuffer payload)
        | _ -> ()
      in
      Array.iter dls ~f:handle_dl
    in
    let dls_proc = Process.create dls_ids dls_proc in
    (* Handle uls *)
    let t_uls = flatten_mat sw_to_t_lmat in
    let sw_uls = flatten_mat sw_to_sw_lmat in
    let uls = Array.append t_uls sw_uls in
    let uls_ids = Array.map uls ~f:Signal.id |> Array.to_list in
    let has_data = ref 1 in
    let uls_proc () =
      let handle_ul ul =
        match !!ul.status with
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
      Array.iter uls ~f:handle_ul
    in
    let uls_proc = Process.create uls_ids uls_proc in
    [| dls_proc; uls_proc |]
  in
  let t_procs = Array.mapi t_to_sw_lmat ~f:term_procs in
  let t_to_s_procs = switch_procs t_to_sw_lmat sw_to_t_lmat sw_to_sw_lmat in
  Array.append t_procs t_to_s_procs
;;

let reduce_scatter conn payload all_link_types all_dst_mats all_link_mats =
  match conn with
  | Conn_type.Ring n ->
    ring_send_receive n payload all_link_types all_dst_mats all_link_mats
  | _ -> [||]
;;
