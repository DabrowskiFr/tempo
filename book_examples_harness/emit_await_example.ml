(* EXPECT: emit,observe:7 *)
open Tempo

let name = "emit_await_example"

let expected = [ "emit"; "observe:7" ]
let trace_max = 8

let run_trace () =
  execute_trace ~instants:4 ~inputs:[ None ] (fun _input output ->
      let s = new_signal () in
      parallel
        [
          (fun () ->
            emit s 7;
            emit output "emit");
          (fun () ->
            let v = await s in
            emit output (Printf.sprintf "observe:%d" v));
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
        let s = new_signal () in
        parallel
          [
            (fun () ->
              emit s 7;
              emit output "emit");
            (fun () ->
              let v = await s in
              emit output (Printf.sprintf "observe:%d" v));
          ])
  in
  List.map
    (fun (i : (unit, string) timeline_instant) ->
      (i.instant, "-", Option.value i.output ~default:"-"))
    timeline
