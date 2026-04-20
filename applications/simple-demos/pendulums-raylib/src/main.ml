open Tempo
open Raylib

type pendulum_state =
  { id : int
  ; theta : float
  ; omega : float
  ; length : float
  ; mass : float
  ; damping : float
  ; anchor_x : float
  ; anchor_y : float
  ; hue : float
  }

type snapshot =
  { pendulums : pendulum_state array
  ; tick : int
  }

type frame =
  { snapshot : snapshot
  ; energy : float
  }

let width = 1360
let height = 840
let pendulums_count = 180
let margin_x = 40.0
let anchor_y = 110.0
let gravity = 560.0
let coupling_k = 34.0
let drive_amp = 0.75
let dt = 0.014

let clampf lo hi v = if v < lo then lo else if v > hi then hi else v

let wrap_angle a = atan2 (sin a) (cos a)

let hue_to_rgb h =
  let h = h -. floor h in
  let h6 = h *. 6.0 in
  let c = 1.0 in
  let x = c *. (1.0 -. abs_float ((mod_float h6 2.0) -. 1.0)) in
  let r, g, b =
    if h6 < 1.0 then c, x, 0.0
    else if h6 < 2.0 then x, c, 0.0
    else if h6 < 3.0 then 0.0, c, x
    else if h6 < 4.0 then 0.0, x, c
    else if h6 < 5.0 then x, 0.0, c
    else c, 0.0, x
  in
  let to_i v = int_of_float (255.0 *. clampf 0.0 1.0 v) in
  to_i r, to_i g, to_i b

let bob_xy p =
  let x = p.anchor_x +. (p.length *. sin p.theta) in
  let y = p.anchor_y +. (p.length *. cos p.theta) in
  (x, y)

let make_initial_pendulum i =
  let span = float_of_int width -. (2.0 *. margin_x) in
  let spacing = span /. float_of_int (pendulums_count - 1) in
  let anchor_x = margin_x +. (float_of_int i *. spacing) in
  let center = 0.5 *. float_of_int (pendulums_count - 1) in
  let d = (float_of_int i -. center) /. center in
  let gaussian = exp (-.6.0 *. d *. d) in
  let chirp = 0.35 *. sin (0.12 *. float_of_int i) in
  let theta = (1.12 *. gaussian) +. chirp +. ((Random.float 2.0 -. 1.0) *. 0.03) in
  let omega = (Random.float 2.0 -. 1.0) *. 0.06 in
  let length = 165.0 +. (24.0 *. sin (0.11 *. float_of_int i)) in
  let mass = 0.85 +. (0.55 *. Random.float 1.0) in
  let damping = 0.010 +. (0.004 *. Random.float 1.0) in
  { id = i
  ; theta
  ; omega
  ; length
  ; mass
  ; damping
  ; anchor_x
  ; anchor_y
  ; hue = float_of_int i /. float_of_int pendulums_count
  }

let local_coupling p snapshot =
  let arr = snapshot.pendulums in
  let left_theta = if p.id = 0 then p.theta else arr.(p.id - 1).theta in
  let right_theta = if p.id = pendulums_count - 1 then p.theta else arr.(p.id + 1).theta in
  (sin (left_theta -. p.theta)) +. (sin (right_theta -. p.theta))

let step_pendulum p snapshot =
  let t = float_of_int snapshot.tick in
  let drive =
    if p.id mod 23 = 0 then drive_amp *. sin ((0.011 *. t) +. (float_of_int p.id *. 0.23)) else 0.0
  in
  let coupling = coupling_k *. local_coupling p snapshot in
  let grav = -.((gravity /. p.length) *. sin p.theta) in
  let damp = -.((p.damping /. p.mass) *. p.omega) in
  let alpha = grav +. damp +. coupling +. drive in
  let omega = p.omega +. (alpha *. dt) in
  let theta = wrap_angle (p.theta +. (omega *. dt)) in
  { p with theta; omega }

let rec handle_input input_signal stop () =
  match await input_signal with
  | 'q' -> emit stop ()
  | _ ->
      pause ();
      handle_input input_signal stop ()

let pendulum_behavior stop snapshot_sig updates_sig init_state =
  watch stop (fun () ->
      let rec loop state =
        let snap = await snapshot_sig in
        let next = step_pendulum state snap in
        emit updates_sig next;
        pause ();
        loop next
      in
      loop init_state)

let frame_collector stop updates_sig snapshot_sig output_signal init_snapshot =
  watch stop (fun () ->
      emit snapshot_sig init_snapshot;
      emit output_signal { snapshot = init_snapshot; energy = 0.0 };
      let rec loop snap =
        let updates = await updates_sig in
        let next_arr = Array.copy snap.pendulums in
        List.iter (fun p -> next_arr.(p.id) <- p) updates;
        let energy =
          Array.fold_left
            (fun acc p ->
              let kinetic = 0.5 *. p.mass *. (p.length *. p.omega) *. (p.length *. p.omega) in
              let potential = p.mass *. gravity *. p.length *. (1.0 -. cos p.theta) in
              acc +. kinetic +. potential)
            0.0 next_arr
        in
        let next_snapshot = { pendulums = next_arr; tick = snap.tick + 1 } in
        emit output_signal { snapshot = next_snapshot; energy };
        emit snapshot_sig next_snapshot;
        pause ();
        loop next_snapshot
      in
      loop init_snapshot)

let scenario input_signal output_signal =
  let stop = new_signal () in
  let snapshot_sig = new_signal () in
  let updates_sig = new_signal_agg ~initial:[] ~combine:(fun acc p -> p :: acc) in
  let init_arr = Array.init pendulums_count make_initial_pendulum in
  let init_snapshot = { pendulums = init_arr; tick = 0 } in
  watch stop (fun () ->
      parallel
        ((fun () -> handle_input input_signal stop ())
         :: (fun () -> frame_collector stop updates_sig snapshot_sig output_signal init_snapshot)
         :: Array.to_list
              (Array.map
                 (fun p -> fun () -> pendulum_behavior stop snapshot_sig updates_sig p)
                 init_arr)))

let draw_frame frame =
  begin_drawing ();
  clear_background (Color.create 7 10 22 255);
  let arr = frame.snapshot.pendulums in
  (* Coupling links between adjacent bobs. *)
  for i = 0 to Array.length arr - 2 do
    let p1 = arr.(i) in
    let p2 = arr.(i + 1) in
    let x1, y1 = bob_xy p1 in
    let x2, y2 = bob_xy p2 in
    let phase = abs_float (p2.theta -. p1.theta) in
    let t = clampf 0.0 1.0 (phase /. 1.4) in
    let r = 35 + int_of_float (180.0 *. t) in
    let g = 95 - int_of_float (45.0 *. t) in
    let b = 170 + int_of_float (60.0 *. (1.0 -. t)) in
    draw_line (int_of_float x1) (int_of_float y1) (int_of_float x2) (int_of_float y2)
      (Color.create r g b 110)
  done;
  Array.iter
    (fun p ->
      let bx, by = bob_xy p in
      draw_line (int_of_float p.anchor_x) (int_of_float p.anchor_y) (int_of_float bx)
        (int_of_float by) (Color.create 145 150 170 255);
      let speed_t = clampf 0.0 1.0 (abs_float p.omega /. 5.0) in
      let hue = mod_float (p.hue +. (0.30 *. speed_t) +. (0.0007 *. by)) 1.0 in
      let br, bg, bb = hue_to_rgb hue in
      draw_circle (int_of_float bx) (int_of_float by) 4.0 (Color.create br bg bb 255))
    arr;
  draw_text "Q or ESC: quit" 12 10 20 (Color.create 220 238 255 255);
  end_drawing ()

let () =
  Random.self_init ();
  init_window width height "Tempo coupled pendulums (Raylib)";
  set_target_fps 60;
  let frames = ref 0 in
  let last_tick = ref (get_time ()) in
  let input () =
    if window_should_close () || is_key_pressed Key.Q || is_key_pressed Key.Escape then Some 'q'
    else None
  in
  let output frame =
    draw_frame frame;
    incr frames;
    let now = get_time () in
    let elapsed = now -. !last_tick in
    if elapsed >= 1.0 then (
      let fps = float_of_int !frames /. elapsed in
      set_window_title
        (Printf.sprintf
           "Tempo coupled pendulums (Raylib) - %.1f FPS - %d pendulums - E=%.0f"
           fps pendulums_count frame.energy);
      frames := 0;
      last_tick := now)
  in
  execute ~input ~output scenario;
  close_window ()
