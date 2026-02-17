(* EXPECT: pong *)
open Tempo

let name = "execute_trace_example"

type input = [ `Ping | `Stop ]
type output = [ `Pong | `Done ]

let expected = [ `Pong ]
let trace_max = 8

let controller input output =
  let v = await input in
  match v with
  | `Ping -> emit output `Pong
  | `Stop -> emit output `Done

let run_trace () =
  execute_trace ~instants:3 ~inputs:[ Some `Ping; None; None ] controller

let pp = function `Pong -> "pong" | `Done -> "done"

let run () =
  let got = run_trace () in
  if got <> expected then
    failwith
      (Printf.sprintf "%s: expected [%s], got [%s]" name
         (String.concat "; " (List.map pp expected))
         (String.concat "; " (List.map pp got)));
  String.concat "," (List.map pp got)

let trace_rows () =
  let timeline =
    execute_timeline ~instants:3 ~inputs:[ Some `Ping; None; None ] controller
  in
  let pp_input = function `Ping -> "ping" | `Stop -> "stop" in
  List.map
    (fun (i : (input, output) timeline_instant) ->
      ( i.instant,
        Option.fold ~none:"-" ~some:pp_input i.input,
        Option.fold ~none:"-" ~some:pp i.output ))
    timeline
