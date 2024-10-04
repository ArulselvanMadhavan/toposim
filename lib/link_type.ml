open! Base

type t =
  | SwitchToSwitch
  | SwitchToTerminal
  | TerminalToSwitch
[@@deriving equal, compare, sexp_of]
