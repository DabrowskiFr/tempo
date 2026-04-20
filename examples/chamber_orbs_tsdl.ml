open Tempo
open Tsdl

type ui_event = Quit | Pulse | ToggleMirror | CouplingUp | CouplingDown

type orb_state =
  { id : int
  ; x : float
  ; y : float
  ; amp : float
  ; phase : float
  ; hue : float
  ; glow : float
  }

type frame =
  { orbs : orb_state array
  ; coupling : float
  ; mirrored : bool
  ; pulse_left : int
  }

let width = 1280
let height = 820
let center_x = 0.5 *. float_of_int width
let center_y = 0.52 *. float_of_int height
let orbs_count = 18
let tau = 6.283185307179586

let clamp lo hi v = if v < lo then lo else if v > hi then hi else v

let draw_blob renderer x y radius (r, g, b) a =
  ignore (Sdl.set_render_draw_color renderer r g b a);
  for dy = -radius to radius do
    let yy = float_of_int ((radius * radius) - (dy * dy)) in
    let dx = int_of_float (sqrt yy) in
    ignore (Sdl.render_draw_line renderer (x - dx) (y + dy) (x + dx) (y + dy))
  done

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

let draw_background renderer =
  ignore (Sdl.set_render_draw_color renderer 8 10 22 255);
  ignore (Sdl.render_clear renderer);
  for i = 0 to 28 do
    let y = (i * height) / 28 in
    let t = float_of_int i /. 28.0 in
    let r = int_of_float (10.0 +. (10.0 *. t)) in
    let g = int_of_float (12.0 +. (9.0 *. t)) in
    let b = int_of_float (24.0 +. (28.0 *. t)) in
    ignore (Sdl.set_render_draw_color renderer r g b 255);
    ignore (Sdl.render_draw_line renderer 0 y width y)
  done;
  draw_blob renderer (int_of_float center_x) (int_of_float center_y) 210 (26, 30, 64) 36

let draw_frame renderer frame =
  draw_background renderer;
  ignore (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend);
  let n = Array.length frame.orbs in
  for i = 0 to n - 1 do
    let a = frame.orbs.(i) in
    let b = frame.orbs.((i + 1) mod n) in
    let t = clamp 0.0 1.0 ((a.glow +. b.glow) *. 0.5) in
    let rr = 70 + int_of_float (120.0 *. t) in
    let gg = 90 + int_of_float (80.0 *. t) in
    let bb = 170 + int_of_float (60.0 *. (1.0 -. t)) in
    let aa = 42 + int_of_float (90.0 *. t) in
    ignore (Sdl.set_render_draw_color renderer rr gg bb aa);
    ignore
      (Sdl.render_draw_line renderer (int_of_float a.x) (int_of_float a.y)
         (int_of_float b.x) (int_of_float b.y))
  done;
  for i = 0 to n - 1 do
    let o = frame.orbs.(i) in
    let r, g, b = hue_to_rgb o.hue in
    let px = int_of_float o.x in
    let py = int_of_float o.y in
    let g1 = int_of_float (clamp 0.0 255.0 (float_of_int g +. (80.0 *. o.glow))) in
    let b1 = int_of_float (clamp 0.0 255.0 (float_of_int b +. (90.0 *. o.glow))) in
    draw_blob renderer px py (7 + int_of_float (3.0 *. o.glow)) (r, g1, b1)
      (80 + int_of_float (90.0 *. o.glow));
    ignore (Sdl.set_render_draw_color renderer 240 245 255 (90 + int_of_float (100.0 *. o.glow)));
    ignore (Sdl.render_draw_point renderer px py)
  done;
  Sdl.render_present renderer

let rec handle_input input_signal stop pulse_ref coupling_ref mirror_toggle_sig () =
  match await input_signal with
  | Quit -> emit stop ()
  | Pulse ->
      pulse_ref := 26;
      handle_input input_signal stop pulse_ref coupling_ref mirror_toggle_sig ()
  | ToggleMirror ->
      emit mirror_toggle_sig ();
      handle_input input_signal stop pulse_ref coupling_ref mirror_toggle_sig ()
  | CouplingUp ->
      coupling_ref := clamp 0.02 0.40 (!coupling_ref +. 0.02);
      handle_input input_signal stop pulse_ref coupling_ref mirror_toggle_sig ()
  | CouplingDown ->
      coupling_ref := clamp 0.02 0.40 (!coupling_ref -. 0.02);
      handle_input input_signal stop pulse_ref coupling_ref mirror_toggle_sig ()

let pulse_decay stop pulse_ref tick_sig =
  watch stop (fun () ->
      let rec loop () =
        let _ = await tick_sig in
        if !pulse_ref > 0 then pulse_ref := !pulse_ref - 1;
        loop ()
      in
      loop ())

let mirror_mode_manager stop mirror_ref mirror_toggle_sig =
  watch stop (fun () ->
      let rec loop mirrored =
        let _ = await mirror_toggle_sig in
        let next = not mirrored in
        mirror_ref := next;
        loop next
      in
      loop false)

let influence_listener stop influence_sig incoming_ref =
  watch stop (fun () ->
      let rec loop () =
        let v = await influence_sig in
        incoming_ref := v;
        loop ()
      in
      loop ())

let orb_behavior stop tick_sig incoming_ref influence_sigs state_sig pulse_ref coupling_ref
  mirror_ref init_state =
  watch stop (fun () ->
      let n = Array.length influence_sigs in
      let left = (init_state.id + n - 1) mod n in
      let right = (init_state.id + 1) mod n in
      let across = (init_state.id + (n / 2)) mod n in
      let rec loop state =
        let _ = await tick_sig in
        let incoming = !incoming_ref in
        let coupling = if !mirror_ref then -. !coupling_ref else !coupling_ref in
        let pulse_boost =
          if !pulse_ref > 0 then
            let p = float_of_int !pulse_ref /. 26.0 in
            (0.22 +. (0.16 *. sin (state.phase +. (0.5 *. p)))) *. p
          else 0.0
        in
        let amp = clamp 0.0 1.0 ((0.90 *. state.amp) +. (coupling *. incoming) +. pulse_boost) in
        let phase = state.phase +. 0.055 +. (0.090 *. amp) in
        let base_angle = (tau *. float_of_int state.id) /. float_of_int n in
        let wobble = 0.22 *. sin (phase +. (0.7 *. amp)) in
        let radius = 220.0 +. (120.0 *. amp) in
        let x = center_x +. (radius *. cos (base_angle +. wobble)) in
        let y = center_y +. (radius *. sin (base_angle -. (0.6 *. wobble))) in
        let glow = clamp 0.0 1.0 ((0.65 *. amp) +. (0.35 *. abs_float (sin phase))) in
        let hue = mod_float (state.hue +. (0.002 +. (0.006 *. amp))) 1.0 in
        let next = { state with x; y; amp; phase; glow; hue } in
        let wave = amp *. sin phase in
        emit influence_sigs.(left) (0.55 *. wave);
        emit influence_sigs.(right) (0.55 *. wave);
        emit influence_sigs.(across) (0.24 *. wave);
        emit state_sig next;
        loop next
      in
      loop init_state)

let frame_collector stop tick_sig state_sig output_signal coupling_ref mirror_ref
  pulse_ref initial_orbs =
  watch stop (fun () ->
      let current = ref (Array.copy initial_orbs) in
      emit output_signal
        { orbs = !current
        ; coupling = !coupling_ref
        ; mirrored = !mirror_ref
        ; pulse_left = !pulse_ref
        };
      let rec loop () =
        let _ = await tick_sig in
        let states = await state_sig in
        if states <> [] then begin
          let arr = Array.copy !current in
          List.iter (fun s -> arr.(s.id) <- s) states;
          current := arr
        end;
        emit output_signal
          { orbs = !current
          ; coupling = !coupling_ref
          ; mirrored = !mirror_ref
          ; pulse_left = !pulse_ref
          };
        loop ()
      in
      loop ())

let clock stop tick_sig =
  watch stop (fun () ->
      let rec loop () =
        emit tick_sig ();
        pause ();
        loop ()
      in
      loop ())

let scenario input_signal output_signal =
  let stop = new_signal () in
  let tick_sig = new_signal () in
  let mirror_toggle_sig = new_signal () in
  let state_sig = new_signal_agg ~initial:[] ~combine:(fun acc s -> s :: acc) in
  let influence_sigs =
    Array.init orbs_count (fun _ -> new_signal_agg ~initial:0.0 ~combine:( +. ))
  in
  let incoming_refs = Array.init orbs_count (fun _ -> ref 0.0) in
  let pulse_ref = ref 0 in
  let coupling_ref = ref 0.16 in
  let mirror_ref = ref false in
  let initial_orbs =
    Array.init orbs_count (fun i ->
        let angle = (tau *. float_of_int i) /. float_of_int orbs_count in
        let x = center_x +. (220.0 *. cos angle) in
        let y = center_y +. (220.0 *. sin angle) in
        { id = i
        ; x
        ; y
        ; amp = 0.35 +. (0.15 *. sin (float_of_int i *. 0.7))
        ; phase = float_of_int i *. 0.33
        ; hue = (0.52 +. (float_of_int i /. float_of_int orbs_count) *. 0.26)
        ; glow = 0.4
        })
  in
  watch stop (fun () ->
      parallel
        ((fun () -> clock stop tick_sig)
         :: (fun () ->
              handle_input input_signal stop pulse_ref coupling_ref mirror_toggle_sig ())
         :: (fun () -> pulse_decay stop pulse_ref tick_sig)
         :: (fun () -> mirror_mode_manager stop mirror_ref mirror_toggle_sig)
         :: (fun () ->
              frame_collector stop tick_sig state_sig output_signal coupling_ref
                mirror_ref pulse_ref initial_orbs)
         :: Array.to_list
              (Array.mapi
                 (fun i _ ->
                    fun () ->
                      influence_listener stop influence_sigs.(i) incoming_refs.(i))
                 initial_orbs)
         @ Array.to_list
             (Array.mapi
                (fun i s ->
                   fun () ->
                     emit state_sig s;
                     orb_behavior stop tick_sig incoming_refs.(i) influence_sigs
                       state_sig pulse_ref coupling_ref mirror_ref s)
                initial_orbs)))

let () =
  Random.self_init ();
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) ->
      Sdl.log "SDL init error: %s" e;
      exit 1
  | Ok () ->
      match
        Sdl.create_window ~w:width ~h:height "Tempo chamber orbs (SDL)"
          Sdl.Window.windowed
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
                  | `Quit -> Some Quit
                  | `Key_down ->
                      let sc = Sdl.Event.get event Sdl.Event.keyboard_scancode in
                      let kc = Sdl.Event.get event Sdl.Event.keyboard_keycode in
                      if sc = Sdl.Scancode.q || kc = Sdl.K.q || kc = Sdl.K.escape
                      then Some Quit
                      else if kc = Sdl.K.space then Some Pulse
                      else if kc = int_of_char 'm' || kc = int_of_char 'M' then Some ToggleMirror
                      else if kc = Sdl.K.up || kc = int_of_char '+' then Some CouplingUp
                      else if kc = Sdl.K.down || kc = int_of_char '-' then Some CouplingDown
                      else None
                  | _ -> None
                else None
              in
              let output frame =
                draw_frame renderer frame;
                incr frames;
                let now = Sdl.get_ticks () in
                let elapsed = Int32.sub now !last_tick in
                if Int32.compare elapsed 1000l >= 0 then (
                  let fps = (float_of_int !frames *. 1000.0) /. Int32.to_float elapsed in
                  Sdl.set_window_title window
                    (Printf.sprintf
                       "Tempo chamber orbs (SDL) - %.1f FPS - %d behaviors - coupling %.2f - mirror %b - pulse %d - [Space/M/Up/Down]"
                       fps orbs_count frame.coupling frame.mirrored frame.pulse_left);
                  frames := 0;
                  last_tick := now)
              in
              execute ~input ~output scenario;
              Sdl.destroy_renderer renderer;
              Sdl.destroy_window window;
              Sdl.quit ())
