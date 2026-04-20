open Tempo
open Tsdl

(* N-body gravitational demo.
   One Tempo behavior per body; a collector aggregates body updates into a
   snapshot used by all bodies at the next logical instant. *)

type body_state =
  { id : int
  ; x : float
  ; y : float
  ; vx : float
  ; vy : float
  ; m : float
  ; hue : float
  }

type frame =
  { bodies : body_state array
  ; kinetic : float
  }

let width = 1280
let height = 800
let bodies_count = 320
let g_const = 1800.
let softening = 36.0
let dt = 0.012
let drag = 0.999
let tau = 6.283185307179586
let center_x = 0.5 *. float_of_int width
let center_y = 0.5 *. float_of_int height

let clamp lo hi v = if v < lo then lo else if v > hi then hi else v

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
  let to_i v = int_of_float (255.0 *. clamp 0.0 1.0 v) in
  to_i r, to_i g, to_i b

let draw_disc renderer x y radius color =
  let cr, cg, cb = color in
  ignore (Sdl.set_render_draw_color renderer cr cg cb 255);
  for dy = -radius to radius do
    let yf = float_of_int (radius * radius - (dy * dy)) in
    let dx = int_of_float (sqrt yf) in
    ignore (Sdl.render_draw_line renderer (x - dx) (y + dy) (x + dx) (y + dy))
  done

let make_initial_body i =
  let r_base = min width height |> float_of_int |> ( *. ) 0.08 in
  let r_span = min width height |> float_of_int |> ( *. ) 0.42 in
  let angle = Random.float tau in
  let radius = r_base +. (r_span *. (Random.float 1.0 ** 1.25)) in
  let x = center_x +. (radius *. cos angle) in
  let y = center_y +. (radius *. sin angle) in
  let orbital = sqrt (g_const *. float_of_int bodies_count /. (radius +. 22.0)) in
  let vx =
    (-.sin angle *. orbital *. (0.70 +. 0.55 *. Random.float 1.0))
    +. (Random.float 2.0 -. 1.0) *. 9.0
  in
  let vy =
    (cos angle *. orbital *. (0.70 +. 0.55 *. Random.float 1.0))
    +. (Random.float 2.0 -. 1.0) *. 9.0
  in
  let m = 0.8 +. 3.5 *. Random.float 1.0 in
  { id = i; x; y; vx; vy; m; hue = Random.float 1.0 }

let compute_forces body snapshot =
  let fx = ref 0.0 in
  let fy = ref 0.0 in
  for i = 0 to Array.length snapshot - 1 do
    let other = snapshot.(i) in
    if other.id <> body.id then (
      let dx = other.x -. body.x in
      let dy = other.y -. body.y in
      let d2 = (dx *. dx) +. (dy *. dy) +. softening in
      let inv_d = 1.0 /. sqrt d2 in
      let inv_d3 = inv_d *. inv_d *. inv_d in
      let f = g_const *. other.m *. inv_d3 in
      fx := !fx +. (dx *. f);
      fy := !fy +. (dy *. f))
  done;
  (* Mild harmonic pull keeps clusters in view and avoids runaway drift. *)
  let to_center_x = center_x -. body.x in
  let to_center_y = center_y -. body.y in
  fx := !fx +. (0.015 *. to_center_x);
  fy := !fy +. (0.015 *. to_center_y);
  !fx, !fy

let step_body body snapshot =
  let ax, ay = compute_forces body snapshot in
  let vx = (body.vx +. (ax *. dt)) *. drag in
  let vy = (body.vy +. (ay *. dt)) *. drag in
  let x = body.x +. (vx *. dt) in
  let y = body.y +. (vy *. dt) in
  { body with x; y; vx; vy }

let rec handle_input input_signal stop () =
  match await input_signal with
  | 'q' -> emit stop ()
  | _ ->
      pause ();
      handle_input input_signal stop ()

let body_behavior stop snapshot_sig updates_sig init_state =
  watch stop (fun () ->
      let rec loop state =
        let snapshot = await snapshot_sig in
        let next = step_body state snapshot in
        emit updates_sig next;
        pause ();
        loop next
      in
      loop init_state)

let frame_collector stop updates_sig snapshot_sig output_signal init_snapshot =
  watch stop (fun () ->
      emit snapshot_sig init_snapshot;
      let rec loop current =
        let updates = await updates_sig in
        let next = Array.copy current in
        List.iter (fun b -> next.(b.id) <- b) updates;
        let kinetic =
          Array.fold_left
            (fun acc b ->
               let v2 = (b.vx *. b.vx) +. (b.vy *. b.vy) in
               acc +. (0.5 *. b.m *. v2))
            0.0 next
        in
        emit output_signal { bodies = next; kinetic };
        emit snapshot_sig next;
        pause ();
        loop next
      in
      loop init_snapshot)

let scenario input_signal output_signal =
  let stop = new_signal () in
  let snapshot_sig = new_signal () in
  let updates_sig = new_signal_agg ~initial:[] ~combine:(fun acc b -> b :: acc) in
  let init_bodies = Array.init bodies_count make_initial_body in
  watch stop (fun () ->
      parallel
        ((fun () -> handle_input input_signal stop ())
         :: (fun () ->
              frame_collector stop updates_sig snapshot_sig output_signal init_bodies)
         :: Array.to_list
              (Array.map
                 (fun body -> fun () -> body_behavior stop snapshot_sig updates_sig body)
                 init_bodies)))

let draw_frame renderer frame =
  ignore (Sdl.set_render_draw_color renderer 8 10 20 255);
  ignore (Sdl.render_clear renderer);
  Array.iter
    (fun b ->
       let speed = sqrt ((b.vx *. b.vx) +. (b.vy *. b.vy)) in
       let speed_t = clamp 0.0 1.0 (speed /. 220.0) in
       let hue = mod_float (b.hue +. (0.23 *. speed_t)) 1.0 in
       let r, g, bl = hue_to_rgb hue in
       let color =
         ( min 255 (r + int_of_float (35.0 *. speed_t))
         , min 255 (g + int_of_float (35.0 *. speed_t))
         , min 255 (bl + int_of_float (35.0 *. speed_t)) )
       in
       let radius = if b.m > 3.6 then 3 else if b.m > 2.0 then 2 else 1 in
       draw_disc renderer (int_of_float b.x) (int_of_float b.y) radius color)
    frame.bodies;
  Sdl.render_present renderer

let () =
  Random.self_init ();
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) ->
      Sdl.log "SDL init error: %s" e;
      exit 1
  | Ok () ->
      match
        Sdl.create_window ~w:width ~h:height "Tempo N-body (SDL)" Sdl.Window.windowed
      with
      | Error (`Msg e) ->
          Sdl.log "SDL window creation error: %s" e;
          exit 1
      | Ok window -> (
          match Sdl.create_renderer window ~index:(-1) ~flags:Sdl.Renderer.accelerated with
          | Error (`Msg e) ->
              Sdl.log "SDL renderer creation error: %s" e;
              Sdl.destroy_window window;
              Sdl.quit ();
              exit 1
          | Ok renderer ->
              let event = Sdl.Event.create () in
              let frames = ref 0 in
              let last_tick = ref (Sdl.get_ticks ()) in
              let input () =
                if Sdl.poll_event (Some event) then
                  match Sdl.Event.(enum (get event typ)) with
                  | `Quit -> Some 'q'
                  | `Key_down ->
                      let sc = Sdl.Event.get event Sdl.Event.keyboard_scancode in
                      let kc = Sdl.Event.get event Sdl.Event.keyboard_keycode in
                      if sc = Sdl.Scancode.q || kc = Sdl.K.q || kc = Sdl.K.escape
                      then Some 'q'
                      else None
                  | _ -> None
                else None
              in
              let output frame =
                draw_frame renderer frame;
                incr frames;
                let now = Sdl.get_ticks () in
                let elapsed_ms = Int32.sub now !last_tick in
                if Int32.compare elapsed_ms 1000l >= 0 then (
                  let fps = (float_of_int !frames *. 1000.0) /. Int32.to_float elapsed_ms in
                  Sdl.set_window_title window
                    (Printf.sprintf
                       "Tempo N-body (SDL) - %.1f FPS - %d bodies - E=%.0f"
                       fps (Array.length frame.bodies) frame.kinetic);
                  frames := 0;
                  last_tick := now)
              in
              execute ~input ~output scenario;
              Sdl.destroy_renderer renderer;
              Sdl.destroy_window window;
              Sdl.quit ())
