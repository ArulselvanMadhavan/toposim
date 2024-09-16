open Base

module Link_signal = struct
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
      | Undefined | Connecting | Ready | Received | Complete -> 1
      | _ -> 0
    ;;

    let make_t src dst status time = { src; dst; status; update_time = time }
  end
end

(* Go through every every xpu dsts. Filter and find links for which this node is the src *)
let downlinks_for_xpu xpu_id dst_mat link_mat =
  let open Option.Let_syntax in
  Array.filter_mapi dst_mat ~f:(fun src_xpu_id dsts ->
    let src_links = link_mat.(src_xpu_id) in
    let%map i, _ = Array.findi dsts ~f:(fun _i d -> Int.(d = xpu_id)) in
    src_links.(i))
;;

let build_trace _coll_name link_mat =
  let open Perfetto.Trace in
  let make_uuid () = Random.bits64 () |> Option.Some in
  (* let timestamp = Int64.of_int 1000 |> Option.Some in *)
  (* let name_field : track_event_name_field = Name "my_event" in *)
  (* let type_ = Some Type_instant in *)
  (* (\* let name_field = default_track_event_name_field atom in *\) *)
  (* let te = default_track_event ~categories:[ "my_cat" ] ~name_field ~type_ () in *)
  (* let data = Track_event te in *)
  (* let tpkt = default_trace_packet ~timestamp ~data () in *)
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
    (* let parent_uuid = uuid in *)
    (* let uuid = make_uuid () in *)
    (* let thread_name = Some coll_name in *)
    (* let thread = *)
    (*   default_thread_descriptor ~pid ~tid:(Int32.of_int 1) ~thread_name () |> Option.Some *)
    (* in *)
    (* let tr_desc = default_track_descriptor ~uuid ~parent_uuid ~thread () in *)
    (* let data = Track_descriptor tr_desc in *)
    (* let thr_pkt = default_trace_packet ~data () in *)
    let event_slice () =
      let type_ = Some Type_slice_begin in
      let track_uuid = uuid in
      let name_field : track_event_name_field = Name "parent" in
      let data = default_track_event ~name_field ~type_ ~track_uuid () |> Track_event in
      let start_pkt =
        default_trace_packet
          ~timestamp_clock_id:(Some Int32.one)
          ~data
          ~optional_trusted_packet_sequence_id
          ()
      in
      let type_ = Some Type_slice_end in
      let data = default_track_event ~name_field ~type_ ~track_uuid () |> Track_event in
      let end_pkt =
        default_trace_packet
          ~timestamp_clock_id:(Int32.of_int 24000)
          ~data
          ~optional_trusted_packet_sequence_id
          ()
      in
      [| start_pkt; end_pkt |]
    in
    let evt_pkts = event_slice () in
    trace_all.(xpu_id) <- Array.append [| proc_pkt; |] evt_pkts);
  (* let trace_all = [| [| tpkt |] |] in *)
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
