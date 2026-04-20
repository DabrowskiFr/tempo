open Tempo
open Tsdl
open Bigarray

type preset = Torch_blue | Blaze_dense

type particle_state =
  { id : int
  ; x : float
  ; y : float
  ; vx : float
  ; vy : float
  ; heat : float
  ; smoke : float
  ; life : float
  ; life0 : float
  ; alive : bool
  ; cooldown : int
  ; seed : int
  }

type snapshot =
  { particles : particle_state array
  ; tick : int
  }

type frame =
  { snapshot : snapshot
  ; active : int
  ; avg_life : float
  }

let width = 1280
let height = 820
let particles_count = 3200
let dt = 0.20
let tau = 6.283185307179586
let emitter_x = 0.5 *. float_of_int width
let emitter_y = float_of_int height -. 74.0

(* Low-res volumetric field for smooth flame rendering. *)
let field_w = 320
let field_h = 206
let field_size = field_w * field_h
let pixel_pitch = field_w * 4
let pixel_bytes = field_size * 4

let clamp lo hi v = if v < lo then lo else if v > hi then hi else v
let lerp a b t = a +. ((b -. a) *. t)

let preset_name = function
  | Torch_blue -> "torche_bleue"
  | Blaze_dense -> "brasier_dense"

let preset_of_env () =
  match
    String.lowercase_ascii
      (Sys.getenv_opt "TEMPO_FIRE_PRESET" |> Option.value ~default:"")
  with
  | "torch" | "blue" | "torche_bleue" | "torch_blue" -> Torch_blue
  | _ -> Blaze_dense

let hash01 n =
  let z = ref (n land 0x7fffffff) in
  z := (!z lxor (!z lsl 13)) land 0x7fffffff;
  z := (!z lxor (!z lsr 17)) land 0x7fffffff;
  z := (!z lxor (!z lsl 5)) land 0x7fffffff;
  float_of_int (!z land 0x7fffffff) /. 2147483647.0

let rand_range seed a b = a +. ((b -. a) *. hash01 seed)

let draw_blob renderer x y radius (r, g, b) alpha =
  ignore (Sdl.set_render_draw_color renderer r g b alpha);
  for dy = -radius to radius do
    let yy = float_of_int ((radius * radius) - (dy * dy)) in
    let dx = int_of_float (sqrt yy) in
    ignore (Sdl.render_draw_line renderer (x - dx) (y + dy) (x + dx) (y + dy))
  done

let respawn preset tick p =
  let seed = p.seed + (tick * 7919) + (p.id * 1291) in
  let jitter_x, jitter_y, ang, speed, heat, smoke, life =
    match preset with
    | Torch_blue ->
        ( rand_range (seed + 1) (-10.0) 10.0
        , rand_range (seed + 2) (-2.0) 2.0
        , rand_range (seed + 3) (-0.22) 0.22
        , rand_range (seed + 4) 16.0 35.0
        , rand_range (seed + 7) 0.90 1.0
        , rand_range (seed + 8) 0.00 0.05
        , rand_range (seed + 9) 64.0 126.0 )
    | Blaze_dense ->
        ( rand_range (seed + 1) (-20.0) 20.0
        , rand_range (seed + 2) (-3.5) 3.5
        , rand_range (seed + 3) (-0.48) 0.48
        , rand_range (seed + 4) 11.0 30.0
        , rand_range (seed + 7) 0.80 1.0
        , rand_range (seed + 8) 0.02 0.12
        , rand_range (seed + 9) 86.0 196.0 )
  in
  let vx = (sin ang *. speed) +. rand_range (seed + 5) (-1.2) 1.2 in
  let vy = -.((cos ang *. speed) +. rand_range (seed + 6) 1.0 5.0) in
  { p with
    x = emitter_x +. jitter_x
  ; y = emitter_y +. jitter_y
  ; vx
  ; vy
  ; heat
  ; smoke
  ; life
  ; life0 = life
  ; alive = true
  ; cooldown = 0
  }

let force_field preset tick x y heat smoke =
  let t = float_of_int tick in
  let altitude = clamp 0.0 1.0 ((emitter_y -. y) /. 470.0) in
  let plume_core = clamp 0.0 1.0 (1.0 -. (abs_float (x -. emitter_x) /. 180.0)) in
  let base_recenter, base_updraft, gust_k, v1_amp, v2_amp =
    match preset with
    | Torch_blue -> 0.040, 18.0, 0.45, 2500.0, -1900.0
    | Blaze_dense -> 0.024, 13.5, 0.90, 5400.0, -4400.0
  in
  let recenter = (emitter_x -. x) *. (base_recenter +. (0.020 *. (1.0 -. altitude))) in
  let updraft = -.((base_updraft +. (5.0 *. plume_core)) *. (0.76 +. (0.24 *. heat))) in
  let swirl1 =
    1.9 *. sin (tau *. ((x /. float_of_int width) *. 0.7 +. (0.00095 *. t)))
  in
  let swirl2 =
    1.4 *. cos (tau *. ((y /. float_of_int height) *. 0.9 -. (0.00110 *. t)))
  in
  let gust_x = (swirl1 +. swirl2) *. gust_k *. (0.35 +. (0.65 *. altitude)) in
  let gust_y =
    0.8 *. gust_k *. sin (tau *. ((x /. float_of_int width) *. 0.55 +. (0.00085 *. t)))
  in
  let vortex cx cy amp =
    let dx = x -. cx in
    let dy = y -. cy in
    let d2 = (dx *. dx) +. (dy *. dy) +. 220.0 in
    (amp *. (-.dy) /. d2, amp *. dx /. d2)
  in
  let cx1 = emitter_x +. (58.0 *. sin (0.0053 *. t)) in
  let cy1 = emitter_y -. (170.0 +. (40.0 *. cos (0.0037 *. t))) in
  let cx2 = emitter_x -. (72.0 *. cos (0.0049 *. t +. 1.1)) in
  let cy2 = emitter_y -. (278.0 +. (62.0 *. sin (0.0041 *. t +. 0.6))) in
  let v1x, v1y = vortex cx1 cy1 v1_amp in
  let v2x, v2y = vortex cx2 cy2 v2_amp in
  let smoke_drag = (match preset with Torch_blue -> 0.11 | Blaze_dense -> 0.20) *. smoke in
  ( recenter +. gust_x +. v1x +. v2x -. ((smoke_drag *. x) /. float_of_int width)
  , updraft +. gust_y +. v1y +. v2y )

let step_particle preset snap p =
  if not p.alive then
    if p.cooldown <= 0 then respawn preset snap.tick p
    else { p with cooldown = p.cooldown - 1 }
  else
    let fx, fy = force_field preset snap.tick p.x p.y p.heat p.smoke in
    let buoyancy_k, drag_k, cool_k, smoke_hot_k, smoke_cold_k =
      match preset with
      | Torch_blue -> 8.8, 0.020, 0.0048, 0.0028, 0.0070
      | Blaze_dense -> 7.1, 0.026, 0.0036, 0.0068, 0.0120
    in
    let buoyancy = -.((buoyancy_k *. p.heat) +. (3.0 *. p.smoke) +. 1.0) in
    let drag = drag_k +. (0.020 *. p.smoke) +. (0.010 *. (1.0 -. p.heat)) in
    let vx = p.vx +. ((fx -. (drag *. p.vx)) *. dt) in
    let vy = p.vy +. ((fy +. buoyancy -. (drag *. p.vy)) *. dt) in
    let x = p.x +. (vx *. dt) in
    let y = p.y +. (vy *. dt) in
    let altitude = clamp 0.0 1.0 ((emitter_y -. y) /. 520.0) in
    let heat_loss = cool_k +. (0.0024 *. p.smoke) +. (0.0024 *. altitude) in
    let heat = clamp 0.0 1.0 (p.heat -. heat_loss) in
    let smoke_grow =
      if p.heat > 0.60 then smoke_hot_k +. (0.0020 *. altitude)
      else smoke_cold_k +. (0.0030 *. altitude)
    in
    let smoke = clamp 0.0 1.0 (p.smoke +. smoke_grow -. 0.0028) in
    let life_drain = match preset with Torch_blue -> 1.20 | Blaze_dense -> 0.92 in
    let life = p.life -. (life_drain +. (0.30 *. altitude)) in
    let out =
      x < -24.0 || x > float_of_int width +. 24.0 || y < -34.0
      || y > float_of_int height +. 24.0
    in
    if life <= 0.0 || out || (heat < 0.015 && smoke < 0.04) then
      let next_cooldown =
        int_of_float (rand_range (p.seed + snap.tick + (p.id * 97)) 0.0 26.0)
      in
      { p with
        x
      ; y
      ; vx
      ; vy
      ; heat
      ; smoke
      ; life = 0.0
      ; alive = false
      ; cooldown = next_cooldown
      }
    else
      { p with x; y; vx; vy; heat; smoke; life }

let rec handle_input input_signal stop preset_ref () =
  match await input_signal with
  | 'q' -> emit stop ()
  | '1' ->
      preset_ref := Torch_blue;
      pause ();
      handle_input input_signal stop preset_ref ()
  | '2' ->
      preset_ref := Blaze_dense;
      pause ();
      handle_input input_signal stop preset_ref ()
  | _ ->
      pause ();
      handle_input input_signal stop preset_ref ()

let particle_behavior stop snapshot_sig updates_sig preset_ref init_state =
  watch stop (fun () ->
      let rec loop state =
        let snap = await snapshot_sig in
        let next = step_particle !preset_ref snap state in
        emit updates_sig next;
        pause ();
        loop next
      in
      loop init_state)

let frame_collector stop updates_sig snapshot_sig output_signal init_snapshot =
  watch stop (fun () ->
      emit snapshot_sig init_snapshot;
      let rec loop snap =
        let updates = await updates_sig in
        let next_arr = Array.copy snap.particles in
        List.iter (fun p -> next_arr.(p.id) <- p) updates;
        let active, life_sum =
          Array.fold_left
            (fun (n, acc) p -> if p.alive then n + 1, acc +. p.life else n, acc)
            (0, 0.0) next_arr
        in
        let avg_life = if active = 0 then 0.0 else life_sum /. float_of_int active in
        let next_snapshot = { particles = next_arr; tick = snap.tick + 1 } in
        emit output_signal { snapshot = next_snapshot; active; avg_life };
        emit snapshot_sig next_snapshot;
        pause ();
        loop next_snapshot
      in
      loop init_snapshot)

let init_particle preset i =
  let seed = (104729 * (i + 1)) lxor ((i * 8191) + 12345) in
  let base =
    { id = i
    ; x = emitter_x
    ; y = emitter_y
    ; vx = 0.0
    ; vy = 0.0
    ; heat = 0.0
    ; smoke = 0.0
    ; life = 0.0
    ; life0 = 0.0
    ; alive = false
    ; cooldown = i mod 37
    ; seed
    }
  in
  let stride = match preset with Torch_blue -> 4 | Blaze_dense -> 6 in
  if i mod stride = 0 then respawn preset 0 base else base

let draw_background renderer preset tick =
  (match preset with
   | Torch_blue -> ignore (Sdl.set_render_draw_color renderer 4 9 18 255)
   | Blaze_dense -> ignore (Sdl.set_render_draw_color renderer 6 8 16 255));
  ignore (Sdl.render_clear renderer);
  for i = 0 to 34 do
    let y = (i * height) / 34 in
    let t = float_of_int i /. 34.0 in
    let r, g, b =
      match preset with
      | Torch_blue ->
          ( int_of_float (lerp 7.0 16.0 t)
          , int_of_float (lerp 14.0 28.0 t)
          , int_of_float (lerp 28.0 58.0 t) )
      | Blaze_dense ->
          ( int_of_float (lerp 10.0 22.0 t)
          , int_of_float (lerp 12.0 18.0 t)
          , int_of_float (lerp 20.0 44.0 t) )
    in
    ignore (Sdl.set_render_draw_color renderer r g b 255);
    ignore (Sdl.render_draw_line renderer 0 y width y)
  done;
  let pulse = 0.72 +. (0.28 *. sin (0.026 *. float_of_int tick)) in
  (match preset with
   | Torch_blue ->
       draw_blob renderer (int_of_float emitter_x) (int_of_float emitter_y + 2) 34
         ( int_of_float (88.0 *. pulse)
         , int_of_float (145.0 *. pulse)
         , int_of_float (240.0 *. pulse) )
         120;
       draw_blob renderer (int_of_float emitter_x) (int_of_float emitter_y + 4) 14
         (170, 230, 255) 150
   | Blaze_dense ->
       draw_blob renderer (int_of_float emitter_x) (int_of_float emitter_y + 2) 46
         ( int_of_float (130.0 *. pulse)
         , int_of_float (70.0 *. pulse)
         , int_of_float (22.0 *. pulse) )
         125;
       draw_blob renderer (int_of_float emitter_x) (int_of_float emitter_y + 4) 20
         (255, 200, 95) 155)

let field_idx x y = (y * field_w) + x

let clear_field a = Array.fill a 0 field_size 0.0

let deposit_fields preset particles heat_field smoke_field =
  let kernel =
    [| 0.05; 0.12; 0.05
     ; 0.12; 0.32; 0.12
     ; 0.05; 0.12; 0.05
    |]
  in
  let heat_gain, smoke_gain =
    match preset with
    | Torch_blue -> 1.9, 0.65
    | Blaze_dense -> 1.55, 1.15
  in
  Array.iter
    (fun p ->
       if p.alive then (
         let fx =
           int_of_float
             (clamp 1.0 (float_of_int (field_w - 2))
                ((p.x /. float_of_int width) *. float_of_int field_w))
         in
         let fy =
           int_of_float
             (clamp 1.0 (float_of_int (field_h - 2))
                ((p.y /. float_of_int height) *. float_of_int field_h))
         in
         let age =
           if p.life0 <= 1e-6
           then 1.0
           else clamp 0.0 1.0 (1.0 -. (p.life /. p.life0))
         in
         let h = (0.35 +. (0.65 *. p.heat)) *. heat_gain in
         let s = (0.20 +. (0.80 *. p.smoke) +. (0.15 *. age)) *. smoke_gain in
         let k = ref 0 in
         for oy = -1 to 1 do
           for ox = -1 to 1 do
             let idx = field_idx (fx + ox) (fy + oy) in
             let w = kernel.(!k) in
             heat_field.(idx) <- heat_field.(idx) +. (h *. w);
             smoke_field.(idx) <- smoke_field.(idx) +. (s *. w);
             incr k
           done
         done))
    particles

let blur_field src dst =
  for y = 1 to field_h - 2 do
    for x = 1 to field_w - 2 do
      let i = field_idx x y in
      let n = src.(field_idx x (y - 1)) in
      let s = src.(field_idx x (y + 1)) in
      let w = src.(field_idx (x - 1) y) in
      let e = src.(field_idx (x + 1) y) in
      let nw = src.(field_idx (x - 1) (y - 1)) in
      let ne = src.(field_idx (x + 1) (y - 1)) in
      let sw = src.(field_idx (x - 1) (y + 1)) in
      let se = src.(field_idx (x + 1) (y + 1)) in
      dst.(i) <-
        (0.42 *. src.(i))
        +. (0.09 *. (n +. s +. w +. e))
        +. (0.055 *. (nw +. ne +. sw +. se))
    done
  done

let field_to_pixels preset heat smoke pixels =
  for y = 0 to field_h - 1 do
    for x = 0 to field_w - 1 do
      let i = field_idx x y in
      let hot = clamp 0.0 1.0 heat.(i) in
      let smk = clamp 0.0 1.0 smoke.(i) in
      let altitude = 1.0 -. (float_of_int y /. float_of_int (field_h - 1)) in
      let plume = clamp 0.0 1.0 (hot +. (0.42 *. smk)) in
      let core = clamp 0.0 1.0 ((1.3 *. hot) -. (0.25 *. smk)) in
      let r, g, b =
        match preset with
        | Torch_blue ->
            ( int_of_float
                (255.0 *. clamp 0.0 1.0 ((0.16 *. plume) +. (0.28 *. core)))
            , int_of_float
                (255.0 *. clamp 0.0 1.0 ((0.45 *. plume) +. (0.48 *. core)))
            , int_of_float
                (255.0 *. clamp 0.0 1.0 ((0.78 *. plume) +. (0.30 *. core) +. (0.20 *. smk))) )
        | Blaze_dense ->
            ( int_of_float
                (255.0 *. clamp 0.0 1.0 ((0.86 *. plume) +. (0.52 *. core)))
            , int_of_float
                (255.0 *. clamp 0.0 1.0 ((0.36 *. plume) +. (0.55 *. core)))
            , int_of_float
                (255.0 *. clamp 0.0 1.0 ((0.10 *. plume) +. (0.22 *. smk))) )
      in
      let a =
        int_of_float
          (clamp 0.0 230.0
             ((38.0 +. (195.0 *. plume)) *. (0.60 +. (0.40 *. altitude))))
      in
      let off = (i * 4) in
      Array1.unsafe_set pixels off r;
      Array1.unsafe_set pixels (off + 1) g;
      Array1.unsafe_set pixels (off + 2) b;
      Array1.unsafe_set pixels (off + 3) a
    done
  done

let draw_frame renderer texture pixels heat smoke heat_blur smoke_blur preset frame =
  draw_background renderer preset frame.snapshot.tick;
  clear_field heat;
  clear_field smoke;
  deposit_fields preset frame.snapshot.particles heat smoke;
  blur_field heat heat_blur;
  blur_field smoke smoke_blur;
  field_to_pixels preset heat_blur smoke_blur pixels;
  ignore (Sdl.update_texture texture None pixels pixel_pitch);
  ignore (Sdl.set_texture_blend_mode texture Sdl.Blend.mode_blend);
  ignore (Sdl.render_copy renderer texture);
  (* Keep a thin spark layer for detail without reintroducing point-cloud look. *)
  ignore (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_add);
  Array.iter
    (fun p ->
       if p.alive && p.heat > 0.25 then (
         let px = int_of_float p.x in
         let py = int_of_float p.y in
         let rr, gg, bb =
           match preset with
           | Torch_blue ->
               ( int_of_float (clamp 0.0 255.0 (110.0 +. (60.0 *. p.heat)))
               , int_of_float (clamp 0.0 255.0 (180.0 +. (75.0 *. p.heat)))
               , 255 )
           | Blaze_dense ->
               ( 255
               , int_of_float (clamp 0.0 255.0 (95.0 +. (145.0 *. p.heat)))
               , int_of_float (clamp 0.0 255.0 (20.0 +. (62.0 *. p.heat))) )
         in
         ignore (Sdl.set_render_draw_color renderer rr gg bb 24);
         ignore
           (Sdl.render_draw_line renderer px py (px - int_of_float (0.35 *. p.vx))
              (py + 2))))
    frame.snapshot.particles;
  ignore (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend);
  Sdl.render_present renderer

let scenario preset_ref input_signal output_signal =
  let stop = new_signal () in
  let snapshot_sig = new_signal () in
  let updates_sig = new_signal_agg ~initial:[] ~combine:(fun acc p -> p :: acc) in
  let init_arr = Array.init particles_count (init_particle !preset_ref) in
  let init_snapshot = { particles = init_arr; tick = 0 } in
  watch stop (fun () ->
      parallel
        ((fun () -> handle_input input_signal stop preset_ref ())
         :: (fun () -> frame_collector stop updates_sig snapshot_sig output_signal init_snapshot)
         :: Array.to_list
              (Array.map
                 (fun p ->
                    fun () ->
                      particle_behavior stop snapshot_sig updates_sig preset_ref p)
                 init_arr)))

let () =
  Random.self_init ();
  let preset_ref = ref (preset_of_env ()) in
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) ->
      Sdl.log "SDL init error: %s" e;
      exit 1
  | Ok () ->
      match
        Sdl.create_window ~w:width ~h:height "Tempo fire/smoke (SDL)" Sdl.Window.windowed
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
          | Ok renderer -> (
              match
                Sdl.create_texture renderer Sdl.Pixel.format_abgr8888
                  Sdl.Texture.access_streaming ~w:field_w ~h:field_h
              with
              | Error (`Msg e) ->
                  Sdl.log "SDL texture creation error: %s" e;
                  Sdl.destroy_renderer renderer;
                  Sdl.destroy_window window;
                  Sdl.quit ();
                  exit 1
              | Ok texture ->
                  let pixels = Array1.create int8_unsigned c_layout pixel_bytes in
                  let heat = Array.make field_size 0.0 in
                  let smoke = Array.make field_size 0.0 in
                  let heat_blur = Array.make field_size 0.0 in
                  let smoke_blur = Array.make field_size 0.0 in
                  let event = Sdl.Event.create () in
                  let frames = ref 0 in
                  let last_tick = ref (Sdl.get_ticks ()) in
                  let input () =
                    if Sdl.poll_event (Some event) then
                      match Sdl.Event.(enum (get event typ)) with
                      | `Quit -> Some 'q'
                      | `Key_down ->
                          let sc =
                            Sdl.Event.get event Sdl.Event.keyboard_scancode
                          in
                          let kc =
                            Sdl.Event.get event Sdl.Event.keyboard_keycode
                          in
                          if sc = Sdl.Scancode.q || kc = Sdl.K.q
                             || kc = Sdl.K.escape
                          then Some 'q'
                          else if kc = int_of_char '1' then Some '1'
                          else if kc = int_of_char '2' then Some '2'
                          else None
                      | _ -> None
                    else None
                  in
                  let output frame =
                    draw_frame renderer texture pixels heat smoke heat_blur smoke_blur
                      !preset_ref frame;
                    incr frames;
                    let now = Sdl.get_ticks () in
                    let elapsed = Int32.sub now !last_tick in
                    if Int32.compare elapsed 1000l >= 0 then (
                      let fps =
                        (float_of_int !frames *. 1000.0)
                        /. Int32.to_float elapsed
                      in
                      Sdl.set_window_title window
                        (Printf.sprintf
                           "Tempo fire/smoke (SDL) - %s - %.1f FPS - active %d/%d - avg life %.0f - [1/2 preset]"
                           (preset_name !preset_ref) fps frame.active
                           particles_count frame.avg_life);
                      frames := 0;
                      last_tick := now)
                  in
                  execute ~input ~output (scenario preset_ref);
                  Sdl.destroy_texture texture;
                  Sdl.destroy_renderer renderer;
                  Sdl.destroy_window window;
                  Sdl.quit ()))
