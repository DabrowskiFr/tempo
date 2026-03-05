open Tempo
open Graphics

(* -- Balls physics*)

type state = { x : int; y : int; vx : int; vy : int; color : Graphics.color }

let width = 800
let height = 600
let radius = 12

let bounce pos vel max_coord =
  let next = pos + vel in
  if next < radius then (radius + (radius - next), -vel)
  else if next > max_coord - radius then
    (max_coord - radius - (next - (max_coord - radius)), -vel)
  else (next, vel)

let step ({ x; y; vx; vy; _ } as s) =
  let x', vx' = bounce x vx width in
  let y', vy' = bounce y vy height in
  { s with x = x'; y = y'; vx = vx'; vy = vy' }

(* -- Input/Output *)

let input () = if key_pressed () then Some (read_key ()) else None

let output states =
  set_color white;
  fill_rect 0 0 width height;
  List.iter
    (fun s ->
      set_color s.color;
      fill_circle s.x s.y radius)
    states;
  synchronize ();
  Unix.sleepf 0.016

(** -- Processes *)

let drive_ball stop ctrl initial_state frame_sig () =
  watch stop (fun () ->
      let state = ref initial_state in
      let move = loop (fun () -> state := step !state) in
      let emit_loop = loop (fun () -> emit frame_sig !state) in
      parallel [ (fun () -> alternate ctrl move idle); emit_loop ])

let rec handle_input input_signal stop ctrl1 ctrl2 () =
  let c = await input_signal in
  if Char.equal c 'q' then emit stop ()
  else (
    if Char.equal c 'p' then emit ctrl1 ();
    if Char.equal c 'o' then emit ctrl2 ();
    pause ();
    handle_input input_signal stop ctrl1 ctrl2 ())

let render_loop stop output_signal frame_sig _init1 _init2 () =
  watch stop (fun () ->
      let rec loop () =
        let states = await frame_sig in
        emit output_signal (List.rev states);
        pause ();
        loop ()
      in
      loop ())

(** -- Scenario *)

let scenario input_signal output_signal =
  let rec idle () =
    pause ();
    idle ()
  in
  let stop = new_signal () in
  let ctrl1 = new_signal () in
  let ctrl2 = new_signal () in
  let frame_sig = new_signal_agg ~initial:[] ~combine:(fun acc s -> s :: acc) in
  let init1 = { x = 100; y = 100; vx = 3; vy = 2; color = blue } in
  let init2 = { x = 400; y = 300; vx = -2; vy = 3; color = red } in
  watch stop (fun () ->
      parallel
        [
          idle
        ; (* to keep the program alive waiting for inputs*)
          handle_input input_signal stop ctrl1 ctrl2
        ; drive_ball stop ctrl1 init1 frame_sig
        ; drive_ball stop ctrl2 init2 frame_sig
        ; render_loop stop output_signal frame_sig init1 init2
        ])

(** -- Execution *)

let () =
  open_graph (Printf.sprintf " %dx%d" width height);
  set_window_title "Tempo ball bounce";
  auto_synchronize false;
  execute ~input ~output scenario
