open Tempo
open Raylib

(* Cloth / jelly simulation with Tempo.
   - one behavior per node
   - synchronous snapshot exchange through signals
   - aggregate collection of per-node updates each logical instant *)

type node_state =
  { id : int
  ; x : float
  ; y : float
  ; vx : float
  ; vy : float
  ; mass : float
  ; pinned : bool
  ; anchor_x : float
  ; anchor_y : float
  }

type snapshot =
  { nodes : node_state array
  ; tick : int
  }

type frame =
  { snapshot : snapshot
  ; kinetic : float
  }

type spring =
  { a : int
  ; b : int
  ; rest : float
  ; k : float
  }

let width = 1280
let height = 800
let grid_w = 56
let grid_h = 36
let nodes_count = grid_w * grid_h
let spacing = 14.0
let dt = 0.013
let damping = 0.992
let gravity = 330.0

let clampf lo hi v = if v < lo then lo else if v > hi then hi else v
let idx x y = (y * grid_w) + x
let min_x = 24.0
let max_x = float_of_int width -. 24.0
let min_y = 24.0
let max_y = float_of_int height -. 24.0

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

let make_anchors () =
  let cloth_w = float_of_int (grid_w - 1) *. spacing in
  let cloth_h = float_of_int (grid_h - 1) *. spacing in
  let ox = (float_of_int width -. cloth_w) *. 0.5 in
  let oy = (float_of_int height -. cloth_h) *. 0.08 in
  Array.init nodes_count (fun i ->
      let x = i mod grid_w in
      let y = i / grid_w in
      (ox +. (float_of_int x *. spacing), oy +. (float_of_int y *. spacing)))

let build_topology anchors =
  let neighbors = Array.make nodes_count [] in
  let springs = ref [] in
  let add_spring a b k =
    let ax, ay = anchors.(a) in
    let bx, by = anchors.(b) in
    let dx = bx -. ax in
    let dy = by -. ay in
    let rest = sqrt ((dx *. dx) +. (dy *. dy)) in
    springs := { a; b; rest; k } :: !springs;
    neighbors.(a) <- (b, rest, k) :: neighbors.(a);
    neighbors.(b) <- (a, rest, k) :: neighbors.(b)
  in
  for y = 0 to grid_h - 1 do
    for x = 0 to grid_w - 1 do
      let i = idx x y in
      if x + 1 < grid_w then add_spring i (idx (x + 1) y) 210.0;
      if y + 1 < grid_h then add_spring i (idx x (y + 1)) 210.0;
      if x + 1 < grid_w && y + 1 < grid_h then add_spring i (idx (x + 1) (y + 1)) 150.0;
      if x - 1 >= 0 && y + 1 < grid_h then add_spring i (idx (x - 1) (y + 1)) 150.0;
      if x + 2 < grid_w then add_spring i (idx (x + 2) y) 85.0;
      if y + 2 < grid_h then add_spring i (idx x (y + 2)) 85.0
    done
  done;
  (neighbors, Array.of_list !springs)

let make_initial_nodes anchors =
  Array.init nodes_count (fun i ->
      let x = i mod grid_w in
      let y = i / grid_w in
      let ax, ay = anchors.(i) in
      let pinned = y = 0 && (x mod 4 = 0 || x = 0 || x = grid_w - 1) in
      let jitter_x = if pinned then 0.0 else (Random.float 2.0 -. 1.0) *. 2.2 in
      let jitter_y = if pinned then 0.0 else Random.float 2.0 in
      let vx = if pinned then 0.0 else (Random.float 2.0 -. 1.0) *. 10.0 in
      let vy = if pinned then 0.0 else (Random.float 2.0 -. 1.0) *. 6.0 in
      { id = i
      ; x = ax +. jitter_x
      ; y = ay +. jitter_y
      ; vx
      ; vy
      ; mass = 1.0 +. (0.35 *. Random.float 1.0)
      ; pinned
      ; anchor_x = ax
      ; anchor_y = ay
      })

let step_node neighbors state snapshot =
  if state.pinned then { state with x = state.anchor_x; y = state.anchor_y; vx = 0.0; vy = 0.0 }
  else
    let nodes = snapshot.nodes in
    let fx = ref 0.0 in
    let fy = ref 0.0 in
    List.iter
      (fun (j, rest, k) ->
        let other = nodes.(j) in
        let dx = other.x -. state.x in
        let dy = other.y -. state.y in
        let dist2 = (dx *. dx) +. (dy *. dy) +. 1e-9 in
        let dist = sqrt dist2 in
        let stretch = dist -. rest in
        let f = k *. stretch in
        fx := !fx +. ((dx /. dist) *. f);
        fy := !fy +. ((dy /. dist) *. f))
      neighbors;
    let t = float_of_int snapshot.tick in
    let wind_x =
      (130.0 *. sin ((0.0075 *. t) +. (0.018 *. state.y)))
      +. (48.0 *. cos ((0.0053 *. t) +. (0.026 *. state.x)))
    in
    let wind_y = 22.0 *. sin ((0.009 *. t) +. (0.03 *. state.x)) in
    fx := !fx +. wind_x;
    fy := !fy +. gravity +. wind_y;
    let ax = !fx /. state.mass in
    let ay = !fy /. state.mass in
    let vx = (state.vx +. (ax *. dt)) *. damping in
    let vy = (state.vy +. (ay *. dt)) *. damping in
    let x = state.x +. (vx *. dt) in
    let y = state.y +. (vy *. dt) in
    let x, vx =
      if x < min_x then min_x, -.vx *. 0.42
      else if x > max_x then max_x, -.vx *. 0.42
      else x, vx
    in
    let y, vy =
      if y < min_y then min_y, -.vy *. 0.42
      else if y > max_y then max_y, -.vy *. 0.42
      else y, vy
    in
    { state with x; y; vx; vy }

let rec handle_input input_signal stop () =
  match await input_signal with
  | 'q' -> emit stop ()
  | _ ->
      pause ();
      handle_input input_signal stop ()

let node_behavior stop snapshot_sig updates_sig neighbors init_state =
  watch stop (fun () ->
      let rec loop state =
        let snap = await snapshot_sig in
        let next = step_node neighbors state snap in
        emit updates_sig next;
        pause ();
        loop next
      in
      loop init_state)

let frame_collector stop updates_sig snapshot_sig output_signal init_snapshot =
  watch stop (fun () ->
      emit snapshot_sig init_snapshot;
      emit output_signal { snapshot = init_snapshot; kinetic = 0.0 };
      let rec loop snap =
        let updates = await updates_sig in
        let next_nodes = Array.copy snap.nodes in
        List.iter (fun n -> next_nodes.(n.id) <- n) updates;
        let kinetic =
          Array.fold_left
            (fun acc n ->
              let v2 = (n.vx *. n.vx) +. (n.vy *. n.vy) in
              acc +. (0.5 *. n.mass *. v2))
            0.0 next_nodes
        in
        let next_snap = { nodes = next_nodes; tick = snap.tick + 1 } in
        emit output_signal { snapshot = next_snap; kinetic };
        emit snapshot_sig next_snap;
        pause ();
        loop next_snap
      in
      loop init_snapshot)

let scenario neighbors init_nodes input_signal output_signal =
  let stop = new_signal () in
  let snapshot_sig = new_signal () in
  let updates_sig = new_signal_agg ~initial:[] ~combine:(fun acc n -> n :: acc) in
  let init_snapshot = { nodes = init_nodes; tick = 0 } in
  watch stop (fun () ->
      parallel
        ((fun () -> handle_input input_signal stop ())
         :: (fun () -> frame_collector stop updates_sig snapshot_sig output_signal init_snapshot)
         :: Array.to_list
              (Array.mapi
                 (fun i node ->
                   let local_neighbors = neighbors.(i) in
                   fun () -> node_behavior stop snapshot_sig updates_sig local_neighbors node)
                 init_nodes)))

let draw_frame springs frame =
  begin_drawing ();
  clear_background (Color.create 10 12 22 255);
  let nodes = frame.snapshot.nodes in
  Array.iter
    (fun s ->
      let a = nodes.(s.a) in
      let b = nodes.(s.b) in
      let dx = b.x -. a.x in
      let dy = b.y -. a.y in
      let dist = sqrt ((dx *. dx) +. (dy *. dy)) in
      let strain = abs_float ((dist -. s.rest) /. s.rest) in
      let t = clampf 0.0 1.0 (strain *. 3.0) in
      let stiff_t = clampf 0.0 1.0 ((s.k -. 85.0) /. (210.0 -. 85.0)) in
      let base = 55 + int_of_float (120.0 *. (1.0 -. t)) in
      let hot = int_of_float (220.0 *. t) in
      let blue = 160 + int_of_float (70.0 *. stiff_t) in
      draw_line (int_of_float a.x) (int_of_float a.y) (int_of_float b.x) (int_of_float b.y)
        (Color.create (base + (hot / 2)) (base - (hot / 4)) blue 160))
    springs;
  Array.iter
    (fun n ->
      if n.pinned then
        draw_rectangle (int_of_float n.x - 2) (int_of_float n.y - 2) 5 5 (Color.create 255 215 90 255)
      else
        let speed = sqrt ((n.vx *. n.vx) +. (n.vy *. n.vy)) in
        let hue = mod_float (0.58 +. (0.0035 *. speed) +. (0.00015 *. n.y)) 1.0 in
        let r, g, b = hue_to_rgb hue in
        draw_pixel (int_of_float n.x) (int_of_float n.y) (Color.create r g b 255))
    nodes;
  draw_text "Q or ESC: quit" 12 10 20 (Color.create 220 238 255 255);
  end_drawing ()

let () =
  Random.self_init ();
  let anchors = make_anchors () in
  let neighbors, springs = build_topology anchors in
  let init_nodes = make_initial_nodes anchors in
  init_window width height "Tempo cloth/jelly (Raylib)";
  set_target_fps 60;
  let frames = ref 0 in
  let last_tick = ref (get_time ()) in
  let input () =
    if window_should_close () || is_key_pressed Key.Q || is_key_pressed Key.Escape then Some 'q'
    else None
  in
  let output frame =
    draw_frame springs frame;
    incr frames;
    let now = get_time () in
    let elapsed = now -. !last_tick in
    if elapsed >= 1.0 then (
      let fps = float_of_int !frames /. elapsed in
      set_window_title
        (Printf.sprintf
           "Tempo cloth/jelly (Raylib) - %.1f FPS - %d nodes - %d springs - E=%.0f"
           fps nodes_count (Array.length springs) frame.kinetic);
      frames := 0;
      last_tick := now)
  in
  execute ~input ~output (scenario neighbors init_nodes);
  close_window ()
