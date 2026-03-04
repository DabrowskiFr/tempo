(* EXPECT: pong *)
open Tempo

let name = "execute_trace_example"

type input = [ `Ping | `Stop ]
type output = [ `Pong | `Done ]

let expected = [ `Pong ]
let trace_max = 8

let input_of_list xs =
  let st = ref xs in
  fun () ->
    match !st with
    | [] -> None
    | x :: rest ->
        st := rest;
        x

let controller input output =
  let v = await input in
  match v with
  | `Ping -> emit output `Pong
  | `Stop -> emit output `Done

let run_trace () =
  Observe.execute_trace ~instants:3
    ~input:(input_of_list [ Some `Ping; None; None ])
    controller

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
    Observe.execute_timeline ~instants:3
      ~input:(input_of_list [ Some `Ping; None; None ])
      controller
  in
  List.map
    (fun (i : output Observe.timeline_instant) ->
      ( i.instant,
        "-",
        Option.fold ~none:"-" ~some:pp i.output ))
    timeline
