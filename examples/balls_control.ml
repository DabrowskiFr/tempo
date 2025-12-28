open Tempo
open Graphics

(* -- Additional combinator *)

(* Toggle a process on/off each time [toggle] is emitted. Starts stopped. *)

let control (toggle : unit signal) (proc : unit -> unit) =
  let rec loop running =
    if running then (
      await toggle;
      loop false)
    else (
      await toggle;
      let _ = fork (fun () -> watch toggle proc) in
      loop true)
  in loop false

(* -- Balls physics*)

type state = { x : int; y : int; vx : int; vy : int; color : Graphics.color }

let width = 800
let height = 600
let radius = 12

let bounce pos vel max_coord =
  let next = pos + vel in
  if next < radius then (radius + (radius - next), -vel)
  else if next > max_coord - radius then
    ( (max_coord - radius) - (next - (max_coord - radius))
    , -vel )
  else (next, vel)

let step ({ x; y; vx; vy; _ } as s) =
  let x', vx' = bounce x vx width in
  let y', vy' = bounce y vy height in
  { s with x = x'; y = y'; vx = vx'; vy = vy' }

(* -- Input/Output *)

let input () = 
  if key_pressed () then Some (read_key ()) else None

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

let drive_ball stop ctrl state_ref =
  let rec loop state_ref =
    let state = !state_ref in
    state_ref := step state;
    pause ();
    loop state_ref
  in
    watch stop (fun () -> 
      control ctrl (fun () -> 
        loop state_ref))

let rec handle_input input_signal stop ctrl1 ctrl2 =
  let c = await input_signal in
  if Char.equal c 'q' then emit stop ()
  else begin
    if Char.equal c 'p' then emit ctrl1 ();
    if Char.equal c 'o' then emit ctrl2 ();
    pause ();
    handle_input input_signal stop ctrl1 ctrl2
  end

let rec render_loop stop output_signal state_ref1 state_ref2 =
  emit output_signal [!state_ref1; !state_ref2];
  pause ();
  render_loop stop output_signal state_ref1 state_ref2

(** -- Scenario *)

let scenario input_signal output_signal =
  let stop = new_signal () in
  let ctrl1 = new_signal () in
  let ctrl2 = new_signal () in
  let state_ref1 = ref { x = 100; y = 100; vx = 3; vy = 2; color = blue } in
  let state_ref2 = ref { x = 400; y = 300; vx = -2; vy = 3; color = red } in
  emit ctrl1 ();
  emit ctrl2 ();
  watch stop (fun () ->
      parallel
        [ (fun () -> handle_input input_signal stop ctrl1 ctrl2)
        ; (fun () -> drive_ball stop ctrl1 state_ref1)
        ; (fun () -> drive_ball stop ctrl2 state_ref2)
        ; (fun () -> render_loop stop output_signal state_ref1 state_ref2)
        ])

(** -- Execution *)

let () =
    Random.self_init ();
    open_graph (Printf.sprintf " %dx%d" width height);
    set_window_title "Tempo ball bounce";
    auto_synchronize false;
    execute ~input ~output scenario
