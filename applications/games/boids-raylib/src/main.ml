open Tempo
open Raylib

type vec = { x : float; y : float }
type boid = { pos : vec; vel : vec; color : Color.t }

type input_state = {
  quit : bool;
  toggle_down : bool;
  reset : bool;
}

type frame = {
  boids : boid list;
  paused : bool;
}

let width = 960
let height = 640
let n_boids = 42
let max_speed = 2.6
let neighbor_radius = 56.0
let separation_radius = 10.0
let align_weight = 0.03
let coh_weight = 0.01
let sep_weight = 0.06
let jitter = 0.09

let add a b = { x = a.x +. b.x; y = a.y +. b.y }
let sub a b = { x = a.x -. b.x; y = a.y -. b.y }
let scale s v = { x = s *. v.x; y = s *. v.y }

let dist2 a b =
  let dx = a.x -. b.x in
  let dy = a.y -. b.y in
  (dx *. dx) +. (dy *. dy)

let clamp_speed v =
  let n = sqrt ((v.x *. v.x) +. (v.y *. v.y)) in
  if n <= max_speed then v else scale (max_speed /. n) v

let bounce pos vel =
  let px = pos.x +. vel.x in
  let py = pos.y +. vel.y in
  let max_x = float_of_int width in
  let max_y = float_of_int height in
  let vx, px' =
    if px < 0.0 then (-.vel.x, 0.0)
    else if px > max_x then (-.vel.x, max_x)
    else (vel.x, px)
  in
  let vy, py' =
    if py < 0.0 then (-.vel.y, 0.0)
    else if py > max_y then (-.vel.y, max_y)
    else (vel.y, py)
  in
  ({ x = px'; y = py' }, { x = vx; y = vy })

let random_color rng =
  Color.create
    (110 + Rng.int rng 146)
    (110 + Rng.int rng 146)
    (110 + Rng.int rng 146)
    255

let random_vec rng mag =
  let a = Rng.float rng (2.0 *. Float.pi) in
  { x = mag *. cos a; y = mag *. sin a }

let init_boids seed =
  let rng = Rng.create seed in
  List.init n_boids (fun _ ->
      {
        pos = { x = Rng.float rng (float_of_int width); y = Rng.float rng (float_of_int height) };
        vel = random_vec rng 2.0;
        color = random_color rng;
      })

let update_boids boids =
  let boids_ref = ref boids in
  boids_ref :=
    List.map
      (fun state ->
        let neighbors =
          List.filter
            (fun b ->
              let d = dist2 b.pos state.pos in
              d > 0.0 && d < (neighbor_radius *. neighbor_radius))
            boids
        in
        let sep_force =
          List.fold_left
            (fun acc b ->
              let d = dist2 b.pos state.pos in
              if d < (separation_radius *. separation_radius) && d > 0.0 then
                add acc (scale (1.0 /. d) (sub state.pos b.pos))
              else acc)
            { x = 0.0; y = 0.0 }
            neighbors
        in
        let align_force =
          match neighbors with
          | [] -> { x = 0.0; y = 0.0 }
          | _ ->
              let sum = List.fold_left (fun acc b -> add acc b.vel) { x = 0.0; y = 0.0 } neighbors in
              scale (1.0 /. float_of_int (List.length neighbors)) sum
        in
        let coh_force =
          match neighbors with
          | [] -> { x = 0.0; y = 0.0 }
          | _ ->
              let sum = List.fold_left (fun acc b -> add acc b.pos) { x = 0.0; y = 0.0 } neighbors in
              let center = scale (1.0 /. float_of_int (List.length neighbors)) sum in
              sub center state.pos
        in
        let noise =
          let t = float_of_int (get_time () |> int_of_float) in
          { x = sin (state.pos.x *. 0.01 +. t) *. jitter; y = cos (state.pos.y *. 0.01 +. t) *. jitter }
        in
        let vel' =
          state.vel
          |> add (scale sep_weight sep_force)
          |> add (scale align_weight align_force)
          |> add (scale coh_weight coh_force)
          |> add noise
          |> clamp_speed
        in
        let pos', vel'' = bounce state.pos vel' in
        { state with pos = pos'; vel = vel'' })
      boids;
  !boids_ref

let poll_input () =
  if window_should_close () || is_key_pressed Key.Escape then raise Exit;
  Some
    {
      quit = false;
      toggle_down = is_key_down Key.Space;
      reset = is_key_pressed Key.R;
    }

let render (frame : frame) =
  begin_drawing ();
  clear_background (Color.create 18 23 33 255);
  List.iter
    (fun b ->
      draw_circle (int_of_float b.pos.x) (int_of_float b.pos.y) 5.0 b.color;
      draw_circle_lines (int_of_float b.pos.x) (int_of_float b.pos.y) 5.0 (Color.create 14 14 14 180))
    frame.boids;
  draw_text "Boids Raylib - SPACE pause/resume - R reset - ESC quit" 18 12 22 Color.raywhite;
  draw_text (if frame.paused then "PAUSED" else "RUNNING") 18 42 20 (if frame.paused then Color.orange else Color.lime);
  end_drawing ()

let main input output =
  let stop = new_signal () in
  let toggle_edge = new_signal () in
  let paused = new_state false in
  let boids_state = new_state (init_boids 4242) in

  let input_feed () =
    let prev_down = ref false in
    let rec loop () =
      let i = await input in
      if i.quit then emit stop ();
      if i.toggle_down && not !prev_down then emit toggle_edge ();
      prev_down := i.toggle_down;
      if i.reset then set_state boids_state (init_boids (2000 + int_of_float (get_time ())));
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

  let sim_proc () =
    let rec loop () =
      if not (get_state paused) then modify_state boids_state update_boids;
      emit output { boids = get_state boids_state; paused = get_state paused };
      pause ();
      loop ()
    in
    loop ()
  in

  parallel [ (fun () -> watch stop (fun () -> parallel [ input_feed; toggle_proc; sim_proc ])) ]

let () =
  init_window width height "Tempo Boids Raylib";
  set_target_fps 60;
  (try execute ~input:poll_input ~output:render main with Exit -> ());
  close_window ()
