module Sim = Toposim.Simulator
module Signal = Sim.Signal
module Process = Sim.Process
module Async = Sim.Async

module Park_signal = struct
  module Park_value = struct
    type t =
      | Park
      | Drive
      | Undefined
    [@@deriving sexp_of, compare]

    let ( = ) x1 x2 =
      match x1, x2 with
      | Park, Park -> true
      | Drive, Drive -> true
      | _ -> false
    ;;

    let resolve_value =
      `Func
        (fun ~last_value xs ->
          let xs = Core.List.stable_dedup xs ~compare in
          match xs with
          | [] -> last_value
          | [ x ] -> x
          | _ -> Undefined)
    ;;

    let check_value_compatibility _ = ()
    let initial_value = Undefined

    let flip = function
      | Park -> Drive
      | Drive -> Park
      | Undefined -> Park
    ;;
  end
end

let get_delay = function
  | Park_signal.Park_value.Park -> 5
  | Park_signal.Park_value.Drive -> 2
  | Park_signal.Park_value.Undefined -> 0
;;

let make_sim () =
  let park_sig = Signal.create (module Park_signal.Park_value) in
  let park_process () =
    let open Sim in
    let park_sig_val = Park_signal.Park_value.flip !!park_sig in
    let delay = get_delay !!park_sig in
    (park_sig <--- park_sig_val) ~delay
  in
  Sim.(
    create
      [ (* Async.create_process *)
        Process.create [ !&park_sig ] park_process
      ; Debug.print_signal "park_sig_val" park_sig
      ])
;;
