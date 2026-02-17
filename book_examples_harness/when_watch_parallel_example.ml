(* EXPECT: when-fired *)
open Tempo

let name = "when_watch_parallel_example"

let expected = [ "when-fired" ]
let trace_max = 8

let run_trace () =
  execute_trace ~instants:4 ~inputs:[ None ] (fun _input output ->
      let gate = new_signal () in
      let stop = new_signal () in
      parallel
        [
          (fun () -> emit gate ());
          (fun () -> when_ gate (fun () -> emit output "when-fired"));
          (fun () ->
            watch stop (fun () ->
                pause ();
                emit output "late"));
          (fun () -> emit stop ());
        ])

let run () =
  let got = run_trace () in
  if got <> expected then
    failwith
      (Printf.sprintf "%s: expected [%s], got [%s]" name
         (String.concat "; " expected)
         (String.concat "; " got));
  String.concat "," got

let trace_rows () =
  let timeline =
    execute_timeline ~instants:4 ~inputs:[ None; None; None; None ] (fun _input output ->
        let gate = new_signal () in
        let stop = new_signal () in
        parallel
          [
            (fun () -> emit gate ());
            (fun () -> when_ gate (fun () -> emit output "when-fired"));
            (fun () ->
              watch stop (fun () ->
                  pause ();
                  emit output "late"));
            (fun () -> emit stop ());
          ])
  in
  List.map
    (fun (i : (unit, string) timeline_instant) ->
      (i.instant, "-", Option.value i.output ~default:"-"))
    timeline
