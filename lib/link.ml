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
