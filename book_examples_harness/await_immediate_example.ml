(* EXPECT: imm:3 *)
open Tempo

let name = "await_immediate_example"

let expected = [ "imm:3" ]
let trace_max = 8

let input_of_list xs =
  let st = ref xs in
  fun () ->
    match !st with
    | [] -> None
    | x :: rest ->
        st := rest;
        x

let run_trace () =
  Observe.execute_trace ~instants:2 ~input:(input_of_list [ None ]) (fun _input output ->
      let s = new_signal () in
      parallel
        [
          (fun () -> emit s 3);
          (fun () ->
            let v = await_immediate s in
            emit output (Printf.sprintf "imm:%d" v));
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
    Observe.execute_timeline ~instants:2 ~input:(input_of_list [ None; None ]) (fun _input output ->
        let s = new_signal () in
        parallel
          [
            (fun () -> emit s 3);
            (fun () ->
              let v = await_immediate s in
              emit output (Printf.sprintf "imm:%d" v));
          ])
  in
  List.map
    (fun (i : string Observe.timeline_instant) ->
      (i.instant, "-", Option.value i.output ~default:"-"))
    timeline
