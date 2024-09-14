open Base
module Sim = Event_driven_sim.Simulator
module Signal = Sim.Signal
module Process = Sim.Process
module Async = Sim.Async

let update_status link nxt_sts =
  let open Sim in
  let open Link.Link_signal.Link_value in
  let delay = get_delay !!link.status in
  let lv = { !!link with status = nxt_sts; update_time = Async.current_time () } in
  (link <--- lv) ~delay
;;

let all2all_send_receive n payload dst_mat link_mat =
  let open Sim in
  let open Link.Link_signal.Link_value in
  (* Send to all uplinks *)
  let trigger_send xpu_id =
    let payload = payload / n in
    let send_time = payload / 1 in
    let uls = link_mat.(xpu_id) in
    let ul_ids = Array.map uls ~f:Signal.id in
    let send_proc () =
      let handle_send ul =
        match !!ul.status with
        | Ready -> update_status ul (Sending send_time)
        | Receiving -> update_status ul Complete
        | _ -> ()
      in
      Array.iter uls ~f:handle_send
    in
    Process.create (Array.to_list ul_ids) send_proc
  in
  let send_procs = Array.init n ~f:trigger_send in
  let trigger_recv xpu_id =
    let dls = Link.downlinks_for_xpu xpu_id dst_mat link_mat in
    let dl_ids = Array.map dls ~f:Signal.id in
    let recv_proc () =
      let handle_recv dl =
        match !!dl.status with
        | Sending delay ->
          let lv =
            { !!dl with status = Receiving; update_time = Async.current_time () }
          in
          (dl <--- lv) ~delay
        | _ -> ()
      in
      Array.iter dls ~f:handle_recv
    in
    Process.create (Array.to_list dl_ids) recv_proc
  in
  let recv_procs = Array.init n ~f:trigger_recv in
  List.map [ send_procs; recv_procs ] ~f:Array.to_list |> List.concat
;;

let ring_send_receive _n _payload _dst_mat _link_mat = []

let reduce_scatter n conn payload dst_mat link_mat =
  match conn with
  | Conn_type.All2All -> all2all_send_receive n payload dst_mat link_mat
  | Conn_type.Ring -> ring_send_receive n payload dst_mat link_mat
;;
