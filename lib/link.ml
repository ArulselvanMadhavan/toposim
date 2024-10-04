open Base
module Sim = Event_driven_sim.Simulator
module Signal = Sim.Signal

module Link_signal = struct
  let undefined_link_id = -1
  let nextid = ref undefined_link_id
  let signal_ids links = Array.map links ~f:Signal.id |> Array.to_list

  module Link_value = struct
    type link_status =
      | Undefined
      | Connecting
      | Ready
      | NonEmptyBuffer of int
      | Sending of int
      | Received
      | ClearBuffer
    [@@deriving equal, sexp_of, compare]

    type t =
      { id : int
      ; src : int
      ; dst : int
      ; status : link_status
      ; update_time : int
      }
    [@@deriving sexp_of, equal, compare, fields ~getters]

    let undefined =
      { id = undefined_link_id; src = -1; dst = -1; status = Undefined; update_time = -1 }
    ;;

    let ( = ) l1 l2 =
      (* If we miss a field here then updates to that field will not be visible *)
      let { id = id1; src = l1_src; dst = l1_dst; status = l1_st; update_time = l1_ut } =
        l1
      in
      let { id = id2; src = l2_src; dst = l2_dst; status = l2_st; update_time = l2_ut } =
        l2
      in
      Int.(l1_src = l2_src)
      && Int.(l1_dst = l2_dst)
      && equal_link_status l1_st l2_st
      && Int.(l1_ut = l2_ut)
      && Int.(id1 = id2)
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
    let buffer_fill_delay payload = payload / 10
    let buffer_clear_delay = 1
    let link_bw = 1

    let get_delay = function
      | Undefined | Connecting | Ready | Received -> 1
      | NonEmptyBuffer payload -> buffer_fill_delay payload
      | ClearBuffer -> buffer_clear_delay
      | Sending payload -> payload / link_bw
    ;;

    let make_t src dst status time =
      Int.incr nextid;
      { id = !nextid; src; dst; status; update_time = time }
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

let build_trace all_link_types all_link_mats =
  let open Perfetto.Trace in
  let make_uuid () = Random.bits64 () |> Option.Some in
  let uuid = make_uuid () in
  let create_process pid link_type =
    let process_name = Link_type.sexp_of_t link_type |> Sexp.to_string |> Some in
    let threads = all_link_mats.(pid) in
    let pid = Int32.of_int pid in
    let process = default_process_descriptor ~pid ~process_name () |> Some in
    let proc_track = Track_descriptor (default_track_descriptor ~uuid ~process ()) in
    let build_thread tid _ =
      let parent_uuid = uuid in
      let uuid = make_uuid () in
      let thread_name =
        Option.map process_name ~f:(fun pn -> pn ^ "_link_" ^ Int.to_string tid)
      in
      let tid = Int32.of_int tid in
      let static_or_dynamic_name = Static_name (Option.value_exn thread_name) in
      let thread = default_thread_descriptor ~pid ~tid ~thread_name () |> Some in
      ( uuid
      , Track_descriptor
          (default_track_descriptor ~uuid ~parent_uuid ~thread ~static_or_dynamic_name ())
      )
    in
    let thread_tracks = Array.mapi threads ~f:build_thread in
    let track_ids, thread_tracks = Array.unzip thread_tracks in
    let start_packets = Array.append [| proc_track |] thread_tracks in
    let start_packets =
      Array.map start_packets ~f:(fun data -> default_trace_packet ~data ())
    in
    track_ids, start_packets
  in
  let track_mat, start_packets =
    Array.mapi all_link_types ~f:create_process |> Array.unzip
  in
  let track_mat = Utils.flatten_mat track_mat in
  let optional_trusted_packet_sequence_id =
    Trusted_packet_sequence_id (Int32.of_int_exn 1)
  in
  let event_packets =
    Array.mapi track_mat ~f:(fun idx track_uuid ->
      let name_field : track_event_name_field = Name "sending" in
      let type_ = Some Type_slice_begin in
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
          ~timestamp:(Some (Int64.of_int ((idx + 1) * 10)))
          ~data
          ~optional_trusted_packet_sequence_id
          ()
      in
      [| start_pkt; end_pkt |])
  in
  let event_packets = Utils.flatten_mat event_packets in
  let start_packets = Utils.flatten_mat start_packets in
  let packet = Array.append start_packets event_packets |> Array.to_list in
  (* let event_slice () = *)
  (*   let type_ = Some Type_slice_begin in *)
  (*   let track_uuid = uuid in *)
  (*   let name_field : track_event_name_field = Name "parent" in *)
  (*   let data = default_track_event ~name_field ~type_ ~track_uuid () |> Track_event in *)
  (*   let start_pkt = *)
  (*     default_trace_packet *)
  (*       ~timestamp:(Some Int64.one) *)
  (*       ~data *)
  (*       ~optional_trusted_packet_sequence_id *)
  (*       () *)
  (*   in *)
  (*   let type_ = Some Type_slice_end in *)
  (*   let data = default_track_event ~name_field ~type_ ~track_uuid () |> Track_event in *)
  (*   let end_pkt = *)
  (*     default_trace_packet *)
  (*       ~timestamp:(Some (Int64.of_int 24000)) *)
  (*       ~data *)
  (*       ~optional_trusted_packet_sequence_id *)
  (*       () *)
  (*   in *)
  (*   [| start_pkt; end_pkt |] *)
  (* in *)
  (* let evt_pkts = event_slice () in *)

  (* let build_trace _coll_name link_mat = *)
  (*   let open Perfetto.Trace in *)
  (*   let make_uuid () = Random.bits64 () |> Option.Some in *)
  (*   let trace_all = Array.init (Array.length link_mat) ~f:(fun _ -> [||]) in *)
  (*   Array.iteri link_mat ~f:(fun xpu_id _uls -> *)
  (*     let uuid = make_uuid () in *)
  (*     let optional_trusted_packet_sequence_id = *)
  (*       Trusted_packet_sequence_id (Int32.of_int_exn ((31 * xpu_id) + 1)) *)
  (*     in *)
  (*     let process_name = "XPU_" ^ Int.to_string xpu_id |> Option.Some in *)
  (*     let pid = Int32.of_int xpu_id in *)
  (*     let process = default_process_descriptor ~pid ~process_name () |> Option.Some in *)
  (*     let tr_desc = default_track_descriptor ~uuid ~process () in *)
  (*     let data = Track_descriptor tr_desc in *)
  (*     let proc_pkt = default_trace_packet ~data ~optional_trusted_packet_sequence_id () in *)
  (*     let event_slice () = *)
  (*       let type_ = Some Type_slice_begin in *)
  (*       let track_uuid = uuid in *)
  (*       let name_field : track_event_name_field = Name "parent" in *)
  (*       let data = default_track_event ~name_field ~type_ ~track_uuid () |> Track_event in *)
  (*       let start_pkt = *)
  (*         default_trace_packet *)
  (*           ~timestamp:(Some Int64.one) *)
  (*           ~data *)
  (*           ~optional_trusted_packet_sequence_id *)
  (*           () *)
  (*       in *)
  (*       let type_ = Some Type_slice_end in *)
  (*       let data = default_track_event ~name_field ~type_ ~track_uuid () |> Track_event in *)
  (*       let end_pkt = *)
  (*         default_trace_packet *)
  (*           ~timestamp:(Some (Int64.of_int 24000)) *)
  (*           ~data *)
  (*           ~optional_trusted_packet_sequence_id *)
  (*           () *)
  (*       in *)
  (*       [| start_pkt; end_pkt |] *)
  (*     in *)
  (*     let evt_pkts = event_slice () in *)
  (*     trace_all.(xpu_id) <- Array.append [| proc_pkt |] evt_pkts); *)
  (*   let packet = *)
  (*     Array.fold trace_all ~init:[] ~f:(Fn.flip List.cons) *)
  (*     |> List.rev *)
  (*     |> Array.concat *)
  (*     |> Array.to_list *)
  (*   in *)
  let trace = default_trace ~packet () in
  let encoder = Pbrt.Encoder.create () in
  encode_pb_trace trace encoder;
  Stdio.Out_channel.with_file "toposim.perfetto" ~f:(fun oc ->
    Stdio.Out_channel.output_bytes oc (Pbrt.Encoder.to_bytes encoder))
;;
