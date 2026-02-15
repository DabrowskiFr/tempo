open Tempo
open Raylib

type input_state = {
  toggle_down : bool;
}

type frame = {
  planet : int * int;
  moon : int * int;
  paused : bool;
}

let width = 900
let height = 620
let cx = width / 2
let cy = height / 2
let sun_radius = 26
let planet_orbit = 180.0
let moon_orbit = 54.0
let planet_radius = 14.0
let moon_radius = 7.0

let planet_speed = 0.022
let moon_speed = 0.075

let to_xy radius angle =
  let x = float_of_int cx +. (radius *. cos angle) in
  let y = float_of_int cy +. (radius *. sin angle) in
  (int_of_float x, int_of_float y)

let poll_input () =
  if window_should_close () || is_key_pressed Key.Escape then raise Exit;
  Some { toggle_down = is_key_down Key.Space || is_key_down Key.P }

let render (f : frame) =
  begin_drawing ();
  clear_background (Color.create 8 12 24 255);
  draw_circle cx cy (float_of_int sun_radius) (Color.create 255 208 92 255);
  draw_circle_lines cx cy (planet_orbit |> int_of_float |> float_of_int) (Color.create 58 74 110 255);
  draw_circle_lines (fst f.planet) (snd f.planet) (moon_orbit |> int_of_float |> float_of_int) (Color.create 62 90 130 150);
  draw_circle (fst f.planet) (snd f.planet) planet_radius (if f.paused then Color.skyblue else Color.blue);
  draw_circle (fst f.moon) (snd f.moon) moon_radius Color.lightgray;
  draw_text "Solar System Raylib - SPACE/P pause - ESC quit" 16 14 24 Color.raywhite;
  draw_text (if f.paused then "PAUSED" else "RUNNING") 16 44 20 (if f.paused then Color.orange else Color.lime);
  end_drawing ()

let main input output =
  let stop = new_signal () in
  let toggle_edge = new_signal () in
  let paused = new_state false in
  let planet_angle = new_state 0.0 in
  let moon_angle = new_state 0.0 in

  let input_proc () =
    let prev_down = ref false in
    let rec loop () =
      let i = await input in
      if i.toggle_down && not !prev_down then emit toggle_edge ();
      prev_down := i.toggle_down;
      loop ()
    in
    loop ()
  in

  let toggle_proc () =
    let rec loop () =
      let _ = await toggle_edge in
      modify_state paused not;
      loop ()
    in
    loop ()
  in

  let anim_proc () =
    let rec loop () =
      let p_speed = if get_state paused then 0.0 else planet_speed in
      modify_state planet_angle (fun a -> a +. p_speed);
      modify_state moon_angle (fun a -> a +. moon_speed);
      let p_a = get_state planet_angle in
      let m_a = get_state moon_angle in
      let px, py = to_xy planet_orbit p_a in
      let mx = px + int_of_float (moon_orbit *. cos m_a) in
      let my = py + int_of_float (moon_orbit *. sin m_a) in
      emit output { planet = (px, py); moon = (mx, my); paused = get_state paused };
      pause ();
      loop ()
    in
    loop ()
  in

  parallel [ (fun () -> watch stop (fun () -> parallel [ input_proc; toggle_proc; anim_proc ])) ]

let () =
  init_window width height "Tempo Solar System Raylib";
  set_target_fps 60;
  (try execute ~input:poll_input ~output:render main with Exit -> ());
  close_window ()
