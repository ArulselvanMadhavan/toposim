open Base
module Sim = Toposim.Simulator
module Signal = Sim.Signal
module Process = Sim.Process
module Async = Sim.Async

module Link_signal = struct
  module Link_value = struct
    type link_status =
      | Undefined
      | Connecting
      | Ready
      | InUse
    [@@deriving equal, sexp_of, compare]

    type t =
      { src : int
      ; dst : int
      ; status : link_status
      ; update_time : int
      }
    [@@deriving sexp_of, equal, compare, fields ~getters]

    let undefined = { src = -1; dst = -1; status = Undefined; update_time = -1 }

    let ( = ) l1 l2 =
      let { src = l1_src; dst = l1_dst; status = l1_st; update_time = l1_ut } = l1 in
      let { src = l2_src; dst = l2_dst; status = l2_st; update_time = l2_ut } = l2 in
      Int.(l1_src = l2_src)
      && Int.(l1_dst = l2_dst)
      && equal_link_status l1_st l2_st
      && Int.(l1_ut = l2_ut)
    ;;

    let resolve_value =
      `Func
        (fun ~last_value xs ->
          let xs = Core.List.stable_dedup xs ~compare in
          match xs with
          | [] -> last_value
          | [ x ] -> x
          | xs ->
            (* If multiple processes are writing, pick the latest write *)
            Core.List.sort xs ~compare:(fun a b ->
              Int.compare (update_time b) (update_time a))
            |> List.hd_exn)
    ;;

    let check_value_compatibility _ = ()
    let initial_value = undefined

    let get_delay = function
      | Undefined | Connecting | Ready -> 1
      | InUse -> 0
    ;;

    let make_t src dst status time = { src; dst; status; update_time = time }
  end
end

let make_links dsts =
  Array.init (Array.length dsts) ~f:(fun _ ->
    Signal.create (module Link_signal.Link_value))
;;

let make_xpu xpu_id link_mat dst_mat =
  let dsts = dst_mat.(xpu_id) in
  let xpu_process () =
    let open Sim in
    let open Link_signal.Link_value in
    (* Establish uplinks *)
    let establish_conn i u =
      let status, update_time =
        match !!u.status with
        | Undefined -> Connecting, (Async.current_time ())
        | x -> x, !!u.update_time
      in
      let delay = get_delay !!u.status in
      let lv = make_t xpu_id dsts.(i) status update_time in
      (u <--- lv) ~delay;
    in
    let uplinks = link_mat.(xpu_id) in
    Array.iteri uplinks ~f:establish_conn;
    (* Listen on downlinks *)
    let listen_conn d =
      let open Link_signal.Link_value in
      let _x = InUse in
      let dsts_from_d = dst_mat.(d) in
      let uplinks_from_d = link_mat.(d) in
      let dlink_id, _ = Array.findi_exn dsts_from_d ~f:(fun _i a -> Int.(a = xpu_id)) in
      let xpu_dlink = uplinks_from_d.(dlink_id) in
      let handle_dlink dl =
        let open Async in
        let%map () = wait_for_change (Signal.id dl) in
        let delay = get_delay !!dl.status in
        let (status, update_time) =
          match !!dl.status with
          | Connecting -> Ready, current_time ()
          | x -> x, !!dl.update_time
        in
        let new_lv = { !!dl with status; update_time } in
        (dl <--- new_lv) ~delay
      in
      handle_dlink xpu_dlink
    in
    let dl_handlers = Array.map dsts ~f:listen_conn in
    Async.Deferred.all_unit (Array.to_list dl_handlers)
  in
  Async.create_process xpu_process
;;

let () =
  let xpu0_dsts = [| 1 |] in
  let xpu1_dsts = [| 0 |] in
  let ls0 = make_links xpu0_dsts in
  let ls1 = make_links xpu1_dsts in
  let link_mat = [| ls0; ls1 |] in
  let dst_mat = [| xpu0_dsts; xpu1_dsts |] in
  let xpu0 = make_xpu 0 link_mat dst_mat in
  let xpu1 = make_xpu 1 link_mat dst_mat in
  let ds0 = Array.map ls0 ~f:(Sim.Debug.print_signal "xpu0_link") in
  let ds1 = Array.map ls1 ~f:(Sim.Debug.print_signal "xpu1_link") in
  let xpus = [ xpu0; xpu1 ] @ Array.to_list ds0 @ Array.to_list ds1 in
  let xpusim = Sim.create xpus in
  Sim.run xpusim ~time_limit:8
;;
