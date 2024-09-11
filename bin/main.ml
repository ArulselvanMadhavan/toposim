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

    (* All links have src and dest *)
    type t =
      { src : int
      ; dst : int
      ; status : link_status
      }
    [@@deriving sexp_of, equal, compare, fields ~getters]

    let undefined = { src = -1; dst = -1; status = Undefined }

    let ( = ) l1 l2 =
      let { src = l1_src; dst = l1_dst; status = l1_st } = l1 in
      let { src = l2_src; dst = l2_dst; status = l2_st } = l2 in
      Int.(l1_src = l2_src) && Int.(l1_dst = l2_dst) && equal_link_status l1_st l2_st
    ;;

    let resolve_value =
      `Func
        (fun ~last_value xs ->
          let xs = Core.List.stable_dedup xs ~compare in
          match xs with
          | [] -> last_value
          | [ x ] -> x
          | _ -> undefined)
    ;;

    let check_value_compatibility _ = ()
    let initial_value = undefined

    let get_delay = function
      | Undefined | Connecting | Ready -> 1
      | InUse -> 0
    ;;

    let next_state = function
      | Undefined -> Connecting
      | Connecting -> Ready
      | Ready -> InUse
      | InUse -> Ready
    ;;

    let make_t src dst status =
      {src; dst; status}
  end
end

let make_uplinks dsts =
    Array.init (Array.length dsts) ~f:(fun _ ->
      Signal.create (module Link_signal.Link_value))

let make_xpu xpu_id uplinks dsts =
  let uplink_ids = Sim.(Array.map uplinks ~f:(Signal.id)) in
  let xpu_process () =
    let open Sim in
    let open Link_signal.Link_value in
    (* Establish uplinks *)
    let establish_conn i u =
      let status = next_state !!u.status in
      let delay = get_delay !!u.status in
      let lv = make_t xpu_id dsts.(i) status in
      (u <--- lv) ~delay
    in
    Array.iteri uplinks ~f:establish_conn;
    (* Listen on downlinks *)
  in
  Process.create (Array.to_list uplink_ids) xpu_process
;;

let () =
  let xpu0_dsts = [|1|] in
  let xpu1_dsts = [|0|] in
  let ls0 = make_uplinks xpu0_dsts in
  let ls1 = make_uplinks xpu1_dsts in
  let xpu0 = make_xpu 0 ls0 xpu0_dsts in
  let xpu1 = make_xpu 1 ls1 xpu1_dsts in
  let ds0 = Array.map ls0 ~f:(Sim.Debug.print_signal "xpu0_link") in
  let ds1 = Array.map ls1 ~f:(Sim.Debug.print_signal "xpu1_link") in
  let xpus = [ xpu0; xpu1 ] @ Array.to_list ds0 @ Array.to_list ds1 in
  let xpusim = Sim.create xpus in
  Sim.run xpusim ~time_limit:5;
;;
