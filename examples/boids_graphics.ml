open Tempo
open Graphics

(* 2D Boids/particles communicating via signals; no shared references between behaviors. *)

type vec = { x : float; y : float }
type boid = { pos : vec; vel : vec; color : color }

let width = 800
let height = 600
let radius = 2
(* Keep counts/radii modest to maintain speed. *)
let n_boids = 30
let max_speed = 4.0
let neighbor_radius = 30.0
let separation_radius = 8.0
let align_weight = 0.025
let coh_weight = 0.005
let sep_weight = 0.05
let jitter = 0.3

let clamp_speed v =
  let norm = sqrt (v.x *. v.x +. v.y *. v.y) in
  if norm <= max_speed then v
  else
    let s = max_speed /. norm in
    { x = v.x *. s; y = v.y *. s }


(* Bounce on borders: reflect velocity when hitting a wall and clamp position inside. *)
let bounce pos vel =
  let px = pos.x +. vel.x in
  let py = pos.y +. vel.y in
  let vx, px' =
    if px < 0. then (-.vel.x, 0.)
    else if px > float width then (-.vel.x, float width)
    else (vel.x, px)
  in
  let vy, py' =
    if py < 0. then (-.vel.y, 0.)
    else if py > float height then (-.vel.y, float height)
    else (vel.y, py)
  in
  ({ x = px'; y = py' }, { x = vx; y = vy })

let add v1 v2 = { x = v1.x +. v2.x; y = v1.y +. v2.y }
let sub v1 v2 = { x = v1.x -. v2.x; y = v1.y -. v2.y }
let scale s v = { x = s *. v.x; y = s *. v.y }

let dist2 v1 v2 =
  let dx = v1.x -. v2.x in
  let dy = v1.y -. v2.y in
  dx *. dx +. dy *. dy

let random_vec mag =
  let a = Random.float (2. *. Float.pi) in
  { x = mag *. cos a; y = mag *. sin a }

let random_color () =
  let r = 128 + Random.int 128 in
  let g = 128 + Random.int 128 in
  let b = 128 + Random.int 128 in
  rgb r g b

(* Behaviors *)

(* Input loop: map key 'q' to stop. *)
let rec handle_input input_signal stop () =
  match await input_signal with
  | 'q' -> emit stop ()
  | _ -> pause (); handle_input input_signal stop ()

(* Collector: takes aggregated boid states for an instant and emits a single frame
   on [frame_sig] once per instant. Starts by emitting an empty frame. *)
let frame_collector stop state_sig frame_sig =
  watch stop (fun () ->
      emit frame_sig [];
      let rec loop () =
        let frame = await state_sig in
        emit frame_sig frame;
        pause ();
        loop ()
      in
      loop ())

(* One boid behavior: waits for last frame, computes new state, emits to state_sig. *)
let boid_behavior stop frame_sig state_sig init_state =
  watch stop (fun () ->
      let rec loop state =
        let others = await frame_sig in
        let neighbors =
          List.filter (fun b -> dist2 b.pos state.pos > 0. && dist2 b.pos state.pos < neighbor_radius *. neighbor_radius) others
        in
        let sep_force =
          List.fold_left
            (fun acc b ->
               let d = dist2 b.pos state.pos in
               if d < separation_radius *. separation_radius && d > 0. then
                 add acc (scale (1. /. d) (sub state.pos b.pos))
               else acc)
            { x = 0.; y = 0. } neighbors
        in
        let align_force =
          match neighbors with
          | [] -> { x = 0.; y = 0. }
          | _ ->
              let sum =
                List.fold_left (fun acc b -> add acc b.vel) { x = 0.; y = 0. } neighbors
              in
              scale (1. /. float (List.length neighbors)) sum
        in
        let coh_force =
          match neighbors with
          | [] -> { x = 0.; y = 0. }
          | _ ->
              let sum =
                List.fold_left (fun acc b -> add acc b.pos) { x = 0.; y = 0. } neighbors
              in
              let center = scale (1. /. float (List.length neighbors)) sum in
              sub center state.pos
        in
        let jitter_force = random_vec jitter in
        let vel' =
          state.vel
          |> add (scale sep_weight sep_force)
          |> add (scale align_weight align_force)
          |> add (scale coh_weight coh_force)
          |> add jitter_force
          |> clamp_speed
        in
        let pos', vel'' = bounce state.pos vel' in
        emit state_sig state;
        pause ();
        loop { state with pos = pos'; vel = vel'' }
      in
      loop init_state)

let render_loop stop frame_sig () =
  watch stop (fun () ->
      let rec loop () =
        let boids = await frame_sig in
        set_color black;
        fill_rect 0 0 width height;
        List.iter
          (fun b ->
             set_color b.color;
             fill_circle (int_of_float b.pos.x) (int_of_float b.pos.y) radius)
          boids;
        synchronize ();
        loop ()
      in
      loop ())

(* Scenario *)

let scenario input_signal _output_signal =
  let stop = new_signal () in
  let state_sig = new_signal_agg ~initial:[] ~combine:(fun acc b -> b :: acc) in
  let frame_sig = new_signal () in
  let boids =
    List.init n_boids (fun _ ->
        { pos = { x = Random.float (float width); y = Random.float (float height) }
        ; vel = random_vec 2.0
        ; color = random_color ()
        })
  in
  watch stop (fun () ->
      parallel
        ((fun () -> handle_input input_signal stop ()) ::
         (fun () -> frame_collector stop state_sig frame_sig) ::
         (fun () -> render_loop stop frame_sig ()) ::
         List.map (fun b -> fun () -> boid_behavior stop frame_sig state_sig b) boids))

(* Execution *)

let () =
  Random.self_init ();
  open_graph (Printf.sprintf " %dx%d" width height);
  set_window_title "Tempo boids";
  auto_synchronize false;
  let input () = if key_pressed () then Some (read_key ()) else None in
  let output _ = () in
  execute ~input ~output scenario
