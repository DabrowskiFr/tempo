open Tempo
open Graphics

let rec loop p () =
  p ();
  pause ();
  loop p ()

let rec idle () =
  pause ();
  idle ()

(** -- Planet physics *)

type frame = { planet : int * int; moon : int * int }

let width = 640
let height = 480
let cx = width / 2
let cy = height / 2
let sun_radius = 20
let planet_orbit = 140
let moon_orbit = 40
let planet_radius = 12
let moon_radius = 6
let planet_speed = 0.02
let moon_speed = 0.07

let to_xy radius angle =
  let x = float cx +. (radius *. cos angle) in
  let y = float cy +. (radius *. sin angle) in
  (int_of_float x, int_of_float y)

let draw_frame { planet = px, py; moon = mx, my } =
  set_color black;
  fill_rect 0 0 width height;
  set_color (rgb 255 215 0);
  fill_circle cx cy sun_radius;
  set_color (rgb 30 144 255);
  fill_circle px py planet_radius;
  set_color (rgb 192 192 192);
  fill_circle mx my moon_radius;
  synchronize ();
  Unix.sleepf 0.016

let rec handle_input input_signal stop toggle () =
  match await input_signal with
  | 'q' -> emit stop ()
  | 'p' ->
      emit toggle ();
      handle_input input_signal stop toggle ()
  | _ -> handle_input input_signal stop toggle ()

let move_planet planet_angle =
  loop (fun () -> planet_angle := !planet_angle +. planet_speed)

let move_moon moon_angle =
  loop (fun () -> moon_angle := !moon_angle +. moon_speed)

let render_loop planet_angle moon_angle output_signal =
  loop (fun () ->
      let px, py = to_xy (float planet_orbit) !planet_angle in
      let dx = int_of_float (float moon_orbit *. cos !moon_angle) in
      let dy = int_of_float (float moon_orbit *. sin !moon_angle) in
      let mx = px + dx in
      let my = py + dy in
      emit output_signal { planet = (px, py); moon = (mx, my) })

let scenario input_signal output_signal =
  let stop = new_signal () in
  let toggle = new_signal () in
  watch stop (fun () ->
      let planet_angle = ref 0. in
      let moon_angle = ref 0. in
      let pause_planet = idle in
      parallel
        [
          handle_input input_signal stop toggle
        ; (fun () -> alternate toggle (move_planet planet_angle) pause_planet)
        ; move_moon moon_angle
        ; render_loop planet_angle moon_angle output_signal
        ])

let () =
  Random.self_init ();
  open_graph (Printf.sprintf " %dx%d" width height);
  set_window_title "Tempo solar system";
  auto_synchronize false;
  let input () = if key_pressed () then Some (read_key ()) else None in
  let output frame = draw_frame frame in
  execute ~input ~output scenario
