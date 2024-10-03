open Base

module Link_signal = struct
  let undefined_link_id = -1
  let nextid = ref undefined_link_id

  module Link_value = struct
    type link_status =
      | Undefined
      | Connecting
      | Ready
      | Sending of int
      | Received
      | Complete
    [@@deriving equal, sexp_of, compare]

    type t =
      { id : int
      ; src : int
      ; dst : int
      ; buffer : int
      ; status : link_status
      ; update_time : int
      }
    [@@deriving sexp_of, equal, compare, fields ~getters]

    let undefined =
      { id = undefined_link_id
      ; src = -1
      ; dst = -1
      ; status = Undefined
      ; update_time = -1
      ; buffer = 0
      }
    ;;

    let ( = ) l1 l2 =
      (* If we miss a field here then updates to that field will not be visible *)
      let { id = id1
          ; src = l1_src
          ; dst = l1_dst
          ; status = l1_st
          ; update_time = l1_ut
          ; buffer = b1
          }
        =
        l1
      in
      let { id = id2
          ; src = l2_src
          ; dst = l2_dst
          ; status = l2_st
          ; update_time = l2_ut
          ; buffer = b2
          }
        =
        l2
      in
      Int.(l1_src = l2_src)
      && Int.(l1_dst = l2_dst)
      && equal_link_status l1_st l2_st
      && Int.(l1_ut = l2_ut)
      && Int.(id1 = id2)
      && Int.(b1 = b2)
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
      | Undefined | Connecting | Ready | Received | Complete -> 1
      | _ -> 0
    ;;

    let buffer_fill_delay payload = payload / 10
    let buffer_clear_delay = 1

    let make_t src dst status time =
      Int.incr nextid;
      { id = !nextid; src; dst; status; update_time = time; buffer = 0 }
    ;;
  end
end

let pf = Stdio.printf

let downlinks_from_terminals dst_mat link_mat xpu_id =
  let open Option.Let_syntax in
  let total_switches = Array.length dst_mat in
  Array.filter_mapi dst_mat ~f:(fun src_xpu_id dsts ->
    let src_links = link_mat.(src_xpu_id) in
    let term_per_switch = Array.length dsts in
    let switch_id_of_term_id term_idx term_id =
      (term_id - term_idx - total_switches) / term_per_switch
    in
    let dsts = Array.mapi dsts ~f:switch_id_of_term_id in
    let%map i, _ = Array.findi dsts ~f:(fun _i d -> Int.(d = xpu_id)) in
    src_links.(i))
;;

let downlinks_for_switch dst_mat link_mat xpu_id =
  let open Option.Let_syntax in
  Array.filter_mapi dst_mat ~f:(fun src_xpu_id dsts ->
    let src_links = link_mat.(src_xpu_id) in
    let%map i, _ = Array.findi dsts ~f:(fun _i d -> Int.(d = xpu_id)) in
    src_links.(i))
;;

let build_trace _coll_name link_mat =
  let open Perfetto.Trace in
  let make_uuid () = Random.bits64 () |> Option.Some in
  let trace_all = Array.init (Array.length link_mat) ~f:(fun _ -> [||]) in
  Array.iteri link_mat ~f:(fun xpu_id _uls ->
    let uuid = make_uuid () in
    let optional_trusted_packet_sequence_id =
      Trusted_packet_sequence_id (Int32.of_int_exn ((31 * xpu_id) + 1))
    in
    let process_name = "XPU_" ^ Int.to_string xpu_id |> Option.Some in
    let pid = Int32.of_int xpu_id in
    let process = default_process_descriptor ~pid ~process_name () |> Option.Some in
    let tr_desc = default_track_descriptor ~uuid ~process () in
    let data = Track_descriptor tr_desc in
    let proc_pkt = default_trace_packet ~data ~optional_trusted_packet_sequence_id () in
    let event_slice () =
      let type_ = Some Type_slice_begin in
      let track_uuid = uuid in
      let name_field : track_event_name_field = Name "parent" in
      let data = default_track_event ~name_field ~type_ ~track_uuid () |> Track_event in
      let start_pkt =
        default_trace_packet
          ~timestamp:(Some Int64.one)
          ~data
          ~optional_trusted_packet_sequence_id
          ()
      in
      let type_ = Some Type_slice_end in
      let data = default_track_event ~name_field ~type_ ~track_uuid () |> Track_event in
      let end_pkt =
        default_trace_packet
          ~timestamp:(Some (Int64.of_int 24000))
          ~data
          ~optional_trusted_packet_sequence_id
          ()
      in
      [| start_pkt; end_pkt |]
    in
    let evt_pkts = event_slice () in
    trace_all.(xpu_id) <- Array.append [| proc_pkt |] evt_pkts);
  let packet =
    Array.fold trace_all ~init:[] ~f:(Fn.flip List.cons)
    |> List.rev
    |> Array.concat
    |> Array.to_list
  in
  let trace = default_trace ~packet () in
  let encoder = Pbrt.Encoder.create () in
  encode_pb_trace trace encoder;
  Stdio.Out_channel.with_file "toposim.perfetto" ~f:(fun oc ->
    Stdio.Out_channel.output_bytes oc (Pbrt.Encoder.to_bytes encoder))
;;
