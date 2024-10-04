open Base
module Sim = Event_driven_sim.Simulator
module Signal = Sim.Signal
module Process = Sim.Process
module Async = Sim.Async
module Link_signal = Link.Link_signal
open Sim
open Utils

let pf = Stdio.printf

let update_status link nxt_sts =
  let open Link.Link_signal.Link_value in
  let delay = get_delay !!link.status in
  let lv = { !!link with status = nxt_sts; update_time = Async.current_time () } in
  (link <--- lv) ~delay
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
  let term_procs uls dls =
    let uls = flatten_mat uls in
    let dls = flatten_mat dls in
    let dl_ids = Link_signal.signal_ids dls in
    (* When there is state involved, create a process for each uplink *)
    let ul_proc ul_count ul () =
      match !!ul.status with
      | Ready when !ul_count > 0 -> update_status ul (NonEmptyBuffer payload)
      | NonEmptyBuffer payload -> update_status ul (Sending payload)
      | Received ->
        Int.decr ul_count;
        update_status ul ClearBuffer
      | ClearBuffer -> update_status ul Ready
       | _ -> ()
    in
    let dl_proc () =
      let handle_dl dl =
        match !!dl.status with
        | Sending _ -> update_status dl Received
        | _ -> ()
      in
      Array.iter dls ~f:handle_dl
    in
    let ul_procs =
      Array.map uls ~f:(fun ul ->
        let ul_count = ref (n - 1) in
        Process.create [ Signal.id ul ] (ul_proc ul_count ul))
    in
    let dl_procs = [| Process.create dl_ids dl_proc |] in
    Array.append ul_procs dl_procs
  in
  let switch_procs t_to_sw_lmat sw_to_t_lmat sw_to_sw_lmat =
    (* Handle dls *)
    let term_dls = flatten_mat t_to_sw_lmat in
    let sw_dls = flatten_mat sw_to_sw_lmat in
    let dls = Array.append term_dls sw_dls in
    let dls_ids = Link_signal.signal_ids dls in
    let dls_proc () =
      let handle_dl dl =
        match !!dl.status with
        | Sending payload ->
          let sw_id = !!dl.dst in
          let src = !!dl.src in
          let is_terminal = src >= n in
          let next_link =
            if is_terminal then sw_to_sw_lmat.(sw_id) else sw_to_t_lmat.(sw_id)
          in
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
    let uls_proc () =
      let handle_ul ul =
        match !!ul.status with
        | NonEmptyBuffer payload ->
          let send_time = payload / 1 in
          update_status ul (Sending send_time)
        | Received -> update_status ul ClearBuffer
        | ClearBuffer -> update_status ul Ready
        | _ -> ()
      in
      Array.iter uls ~f:handle_ul
    in
    let uls_proc = Process.create uls_ids uls_proc in
    [| dls_proc; uls_proc |]
  in
  let t_procs = term_procs t_to_sw_lmat sw_to_t_lmat in
  let t_to_s_procs = switch_procs t_to_sw_lmat sw_to_t_lmat sw_to_sw_lmat in
  Array.append t_procs t_to_s_procs
;;

let reduce_scatter conn payload all_link_types all_dst_mats all_link_mats =
  match conn with
  | Conn_type.Ring n ->
    ring_send_receive n payload all_link_types all_dst_mats all_link_mats
  | _ -> [||]
;;
