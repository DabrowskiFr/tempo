open Tempo
open Tsdl

type team = Ember | Azure

type ui_event = Quit | Pulse | AggroUp | AggroDown | ShieldBurst

type spell_kind = Bolt | Lance

type cast_event =
  { caster : int
  ; style : int
  ; team : team
  ; x : float
  ; y : float
  ; dirx : float
  ; diry : float
  ; power : float
  ; kind : spell_kind
  }

type hit_event =
  { target : int
  ; source : int
  ; damage : float
  ; stun : int
  ; knockx : float
  ; knocky : float
  }

type mage_state =
  { id : int
  ; role : int
  ; team : team
  ; x : float
  ; y : float
  ; vx : float
  ; vy : float
  ; hp : float
  ; mana : float
  ; shield : float
  ; cooldown : int
  ; stun : int
  ; aura : float
  ; flash : int
  ; alive : bool
  }

type projectile =
  { id : int
  ; owner : int
  ; style : int
  ; team : team
  ; x : float
  ; y : float
  ; vx : float
  ; vy : float
  ; ttl : int
  ; radius : float
  ; damage : float
  ; stun : int
  ; kind : spell_kind
  }

type spark =
  { x : float
  ; y : float
  ; vx : float
  ; vy : float
  ; ttl : int
  ; r : int
  ; g : int
  ; b : int
  }

type projectile_frame =
  { projectiles : projectile list
  ; sparks : spark list
  }

type frame =
  { mages : mage_state array
  ; projectiles : projectile list
  ; sparks : spark list
  ; pulse_left : int
  ; aggression : float
  ; ember_alive : int
  ; azure_alive : int
  }

let width = 1360
let height = 860
let center_x = 0.5 *. float_of_int width
let center_y = 0.53 *. float_of_int height
let arena_radius = 310.0
let mage_radius = 12.0
let mages_per_team = 4
let mages_count = 2 * mages_per_team
let dt = 0.95
let tau = 6.283185307179586

let clamp lo hi v = if v < lo then lo else if v > hi then hi else v

let vlen x y = sqrt ((x *. x) +. (y *. y))

let norm x y =
  let l = vlen x y in
  if l < 1e-6 then 0.0, 0.0 else (x /. l, y /. l)

let draw_blob renderer x y radius (r, g, b) alpha =
  ignore (Sdl.set_render_draw_color renderer r g b alpha);
  for dy = -radius to radius do
    let yy = float_of_int ((radius * radius) - (dy * dy)) in
    let dx = int_of_float (sqrt yy) in
    ignore (Sdl.render_draw_line renderer (x - dx) (y + dy) (x + dx) (y + dy))
  done

let fill_rect renderer x y w h (r, g, b) alpha =
  ignore (Sdl.set_render_draw_color renderer r g b alpha);
  ignore
    (Sdl.render_fill_rect renderer (Some (Sdl.Rect.create ~x ~y ~w ~h)))

let team_color = function
  | Ember -> (245, 125, 45)
  | Azure -> (85, 170, 255)

let other_team = function
  | Ember -> Azure
  | Azure -> Ember

let combine_batches acc batch = List.rev_append batch acc

let draw_background renderer pulse =
  ignore (Sdl.set_render_draw_color renderer 8 9 19 255);
  ignore (Sdl.render_clear renderer);
  for i = 0 to 34 do
    let y = (i * height) / 34 in
    let t = float_of_int i /. 34.0 in
    let r = int_of_float (10.0 +. (14.0 *. t)) in
    let g = int_of_float (11.0 +. (11.0 *. t)) in
    let b = int_of_float (21.0 +. (34.0 *. t)) in
    ignore (Sdl.set_render_draw_color renderer r g b 255);
    ignore (Sdl.render_draw_line renderer 0 y width y)
  done;
  let p = float_of_int pulse /. 32.0 in
  draw_blob renderer (int_of_float center_x) (int_of_float center_y)
    (210 + int_of_float (52.0 *. p))
    (38, 44, 78) (40 + int_of_float (56.0 *. p));
  ignore (Sdl.set_render_draw_color renderer 70 78 118 90);
  ignore
    (Sdl.render_draw_rect renderer
       (Some
          (Sdl.Rect.create
             ~x:(int_of_float (center_x -. arena_radius))
             ~y:(int_of_float (center_y -. arena_radius *. 0.74))
             ~w:(int_of_float (2.0 *. arena_radius))
             ~h:(int_of_float (1.48 *. arena_radius)))));
  ignore (Sdl.set_render_draw_color renderer 105 115 170 45);
  ignore
    (Sdl.render_draw_rect renderer
       (Some
          (Sdl.Rect.create
             ~x:(int_of_float (center_x -. arena_radius *. 0.75))
             ~y:(int_of_float (center_y -. arena_radius *. 0.54))
             ~w:(int_of_float (1.5 *. arena_radius))
             ~h:(int_of_float (1.08 *. arena_radius)))));
  ()

let nearest_enemy (mage : mage_state) (mages : mage_state array) : mage_state option =
  let best = ref None in
  for i = 0 to Array.length mages - 1 do
    let o = mages.(i) in
    if o.alive && o.team = other_team mage.team then (
      let dx = o.x -. mage.x in
      let dy = o.y -. mage.y in
      let d2 = (dx *. dx) +. (dy *. dy) in
      match !best with
      | None -> best := Some (i, d2)
      | Some (_, bd2) -> if d2 < bd2 then best := Some (i, d2))
  done;
  match !best with
  | None -> None
  | Some (i, _) -> Some mages.(i)

let apply_hits (mage : mage_state) (hits : hit_event list) : mage_state =
  List.fold_left
    (fun (s : mage_state) (h : hit_event) ->
       if h.target <> s.id || not s.alive then s
       else
         let shield_absorb = min s.shield h.damage in
         let damage_left = h.damage -. shield_absorb in
         let hp = clamp 0.0 100.0 (s.hp -. damage_left) in
         let alive = hp > 0.0 in
         let source_kick = float_of_int (h.source mod 5) *. 0.003 in
         { s with
           hp
         ; shield = clamp 0.0 100.0 (s.shield -. shield_absorb)
         ; stun = max s.stun h.stun
         ; vx = s.vx +. h.knockx
         ; vy = s.vy +. h.knocky
         ; aura = s.aura +. source_kick
         ; flash = 7
         ; alive
         })
    mage hits

let init_mages () =
  Array.init mages_count (fun i ->
      let team, local_i, base_x =
        if i < mages_per_team
        then Ember, i, center_x -. 170.0
        else Azure, i - mages_per_team, center_x +. 170.0
      in
      let ang =
        (tau *. float_of_int local_i /. float_of_int mages_per_team)
        +. if team = Ember then 0.2 else 0.9
      in
      let x = base_x +. (70.0 *. cos ang) in
      let y = center_y +. (120.0 *. sin ang) in
      { id = i
      ; role = local_i mod 3
      ; team
      ; x
      ; y
      ; vx = 0.0
      ; vy = 0.0
      ; hp = 100.0
      ; mana = 1.0
      ; shield = 34.0
      ; cooldown = 10 + (local_i * 5)
      ; stun = 0
      ; aura = float_of_int local_i *. 0.9
      ; flash = 0
      ; alive = true
      })

let spawn_from_cast (next_id : int) (cast : cast_event) : projectile list * int =
  match cast.kind with
  | Bolt ->
      let p =
        { id = next_id
        ; owner = cast.caster
        ; style = cast.style
        ; team = cast.team
        ; x = cast.x
        ; y = cast.y
        ; vx = cast.dirx *. 11.0
        ; vy = cast.diry *. 11.0
        ; ttl = 88
        ; radius = 9.0
        ; damage = 9.5 *. cast.power
        ; stun = 5
        ; kind = Bolt
        }
      in
      [ p ], next_id + 1
  | Lance ->
      let spawn_one j =
        let spread = (float_of_int (j - 1)) *. 0.17 in
        let c = cos spread in
        let s = sin spread in
        let dx = (cast.dirx *. c) -. (cast.diry *. s) in
        let dy = (cast.dirx *. s) +. (cast.diry *. c) in
        { id = next_id + j
        ; owner = cast.caster
        ; style = cast.style
        ; team = cast.team
        ; x = cast.x
        ; y = cast.y
        ; vx = dx *. 10.0
        ; vy = dy *. 10.0
        ; ttl = 70
        ; radius = 7.0
        ; damage = 7.2 *. cast.power
        ; stun = 3
        ; kind = Lance
        }
      in
      [ spawn_one 0; spawn_one 1; spawn_one 2 ], next_id + 3

let hit_color team =
  match team with
  | Ember -> (255, 150, 90)
  | Azure -> (130, 190, 255)

let spell_palette team style =
  match team, style mod 3 with
  | Ember, 0 -> (255, 142, 85)
  | Ember, 1 -> (255, 212, 120)
  | Ember, _ -> (255, 92, 64)
  | Azure, 0 -> (120, 182, 255)
  | Azure, 1 -> (168, 232, 255)
  | Azure, _ -> (96, 220, 220)

let draw_role_glyph renderer role x y team =
  let tr, tg, tb = team_color team in
  match role mod 3 with
  | 0 ->
      draw_blob renderer x y 3 (tr, tg, tb) 210
  | 1 ->
      fill_rect renderer (x - 3) (y - 3) 7 7 (tr, tg, tb) 210
  | _ ->
      ignore (Sdl.set_render_draw_color renderer tr tg tb 220);
      ignore (Sdl.render_draw_line renderer x (y - 4) (x - 4) (y + 3));
      ignore (Sdl.render_draw_line renderer (x - 4) (y + 3) (x + 4) (y + 3));
      ignore (Sdl.render_draw_line renderer (x + 4) (y + 3) x (y - 4))

let sparks_from_hit team x y =
  let r, g, b = hit_color team in
  List.init 14 (fun i ->
      let a = (tau *. float_of_int i) /. 14.0 in
      let sp = 1.4 +. (0.7 *. sin (float_of_int i *. 1.2)) in
      { x
      ; y
      ; vx = cos a *. sp
      ; vy = sin a *. sp
      ; ttl = 18 + (i mod 9)
      ; r
      ; g
      ; b
      })

let sparks_from_cast team x y =
  let r, g, b = hit_color team in
  List.init 6 (fun i ->
      { x
      ; y
      ; vx = (Random.float 2.0 -. 1.0) *. 1.6
      ; vy = -. Random.float 1.4
      ; ttl = 10 + (i mod 5)
      ; r
      ; g
      ; b
      })

let rec handle_input input_signal stop pulse_ref aggression_ref shield_burst_ref () =
  match await input_signal with
  | Quit -> emit stop ()
  | Pulse ->
      pulse_ref := 32;
      handle_input input_signal stop pulse_ref aggression_ref shield_burst_ref ()
  | AggroUp ->
      aggression_ref := clamp 0.55 2.0 (!aggression_ref +. 0.10);
      handle_input input_signal stop pulse_ref aggression_ref shield_burst_ref ()
  | AggroDown ->
      aggression_ref := clamp 0.55 2.0 (!aggression_ref -. 0.10);
      handle_input input_signal stop pulse_ref aggression_ref shield_burst_ref ()
  | ShieldBurst ->
      shield_burst_ref := 30;
      handle_input input_signal stop pulse_ref aggression_ref shield_burst_ref ()

let clock stop tick_sig cast_sig hit_sig mage_sig pulse_ref shield_burst_ref =
  watch stop (fun () ->
      let rec loop tick =
        emit tick_sig tick;
        emit cast_sig [];
        emit hit_sig [];
        emit mage_sig [];
        if !pulse_ref > 0 then pulse_ref := !pulse_ref - 1;
        if !shield_burst_ref > 0 then shield_burst_ref := !shield_burst_ref - 1;
        pause ();
        loop (tick + 1)
      in
      loop 0)

let cast_listener stop cast_sig casts_ref =
  watch stop (fun () ->
      let rec loop () =
        casts_ref := await cast_sig;
        loop ()
      in
      loop ())

let hit_listener stop hit_sig hits_ref =
  watch stop (fun () ->
      let rec loop () =
        hits_ref := await hit_sig;
        loop ()
      in
      loop ())

let mage_listener stop
  (mage_sig : (mage_state list, mage_state list) agg_signal)
  (mages_ref : mage_state array ref) =
  watch stop (fun () ->
      let rec loop () =
        let batch : mage_state list = await mage_sig in
        if batch <> [] then (
          let next = Array.copy !mages_ref in
          List.iter (fun (s : mage_state) -> next.(s.id) <- s) batch;
          mages_ref := next);
        loop ()
      in
      loop ())

let projectile_listener stop projectile_sig projectile_ref =
  watch stop (fun () ->
      let rec loop () =
        projectile_ref := await projectile_sig;
        loop ()
      in
      loop ())

let mage_behavior stop tick_sig mage_sig cast_sig
  (hits_ref : hit_event list ref) (mages_ref : mage_state array ref)
  (pulse_ref : int ref) (aggression_ref : float ref)
  (shield_burst_ref : int ref) (init_state : mage_state) =
  watch stop (fun () ->
      let home_phase = float_of_int init_state.id *. 0.77 in
      let home_center_x = if init_state.team = Ember then center_x -. 180.0 else center_x +. 180.0 in
      let rec loop state =
        let tick = await tick_sig in
        let hits = !hits_ref in
        let all = !mages_ref in
        let s0 = apply_hits state hits in
        let s1 =
          if not s0.alive then s0
          else
            let target = nearest_enemy s0 all in
            let tx, ty =
              match target with
              | None -> home_center_x, center_y
              | Some t -> t.x, t.y
            in
            let orbit = home_phase +. (0.015 *. float_of_int tick) in
            let home_x = home_center_x +. (78.0 *. cos orbit) in
            let home_y = center_y +. (140.0 *. sin (orbit *. 1.1)) in
            let pull_hx = (home_x -. s0.x) *. 0.010 in
            let pull_hy = (home_y -. s0.y) *. 0.010 in
            let atk_dx = tx -. s0.x in
            let atk_dy = ty -. s0.y in
            let ndx, ndy = norm atk_dx atk_dy in
            let chase = 0.065 *. !aggression_ref in
            let vx =
              (s0.vx *. 0.92)
              +. pull_hx
              +. (chase *. ndx)
              +. (0.018 *. sin (s0.aura +. float_of_int tick *. 0.04))
            in
            let vy =
              (s0.vy *. 0.92)
              +. pull_hy
              +. (chase *. ndy)
              +. (0.016 *. cos (s0.aura +. float_of_int tick *. 0.03))
            in
            let stun = max 0 (s0.stun - 1) in
            let mobile = stun = 0 in
            let vx = if mobile then vx else vx *. 0.72 in
            let vy = if mobile then vy else vy *. 0.72 in
            let x = s0.x +. (vx *. dt) in
            let y = s0.y +. (vy *. dt) in
            let dx = x -. center_x in
            let dy = y -. center_y in
            let dist = vlen dx dy in
            let x, y, vx, vy =
              if dist > arena_radius then
                let nx, ny = norm dx dy in
                let px = center_x +. (nx *. arena_radius) in
                let py = center_y +. (ny *. arena_radius) in
                (px, py, vx -. (0.65 *. nx), vy -. (0.65 *. ny))
              else x, y, vx, vy
            in
            let mana = clamp 0.0 1.0 (s0.mana +. 0.007 +. if !pulse_ref > 0 then 0.005 else 0.0) in
            let shield =
              let base = clamp 0.0 45.0 (s0.shield -. 0.12 +. if !shield_burst_ref > 0 then 0.9 else 0.18) in
              if !pulse_ref > 0 then clamp 0.0 45.0 (base +. 0.12) else base
            in
            let cooldown = max 0 (s0.cooldown - 1) in
            let flash = max 0 (s0.flash - 1) in
            { s0 with
              x
            ; y
            ; vx
            ; vy
            ; mana
            ; shield
            ; cooldown
            ; stun
            ; aura = s0.aura +. 0.03 +. (0.02 *. mana)
            ; flash
            }
        in
        let s2 =
          if not s1.alive || s1.stun > 0 || s1.cooldown > 0 || s1.mana < 0.18 then s1
          else
            let cast_gate = sin (0.115 *. float_of_int tick +. (0.9 *. s1.aura)) in
            if cast_gate < (0.56 -. (0.15 *. !aggression_ref)) then s1
            else
              match nearest_enemy s1 all with
              | None -> s1
              | Some tgt ->
                  let dx = tgt.x -. s1.x in
                  let dy = tgt.y -. s1.y in
                  let dirx, diry = norm dx dy in
                  let lance =
                    if s1.role = 1 then s1.mana > 0.45 && cast_gate > 0.45
                    else s1.mana > 0.63 && cast_gate > 0.78
                  in
                  let kind = if lance then Lance else Bolt in
                  let mana_cost =
                    if lance
                    then if s1.role = 1 then 0.28 else 0.34
                    else if s1.role = 2 then 0.14 else 0.18
                  in
                  let cd =
                    if lance then if s1.role = 1 then 22 else 26
                    else if s1.role = 2 then 10 else 13
                  in
                  emit cast_sig
                    [ { caster = s1.id
                      ; style = s1.role
                      ; team = s1.team
                      ; x = s1.x
                      ; y = s1.y
                      ; dirx
                      ; diry
                      ; power =
                          1.0 +. (0.25 *. !aggression_ref)
                          +. if s1.role = 0 then 0.08 else 0.0
                      ; kind
                      }
                    ];
                  { s1 with mana = clamp 0.0 1.0 (s1.mana -. mana_cost); cooldown = cd; flash = 9 }
        in
        emit mage_sig [ s2 ];
        loop s2
      in
      loop init_state)

let projectile_manager stop tick_sig (casts_ref : cast_event list ref)
  (mages_ref : mage_state array ref) projectile_sig hit_sig =
  watch stop (fun () ->
      let rec loop next_id projectiles sparks =
        let _tick = await tick_sig in
        let casts = !casts_ref in
        let spawned, next_id, cast_sparks =
          List.fold_left
            (fun (acc, nid, sparx) cast ->
               let ps, nid' = spawn_from_cast nid cast in
               let sx = sparks_from_cast cast.team cast.x cast.y in
               (List.rev_append ps acc, nid', List.rev_append sx sparx))
            ([], next_id, []) casts
        in
        let mages = !mages_ref in
        let rec step_projectiles (ps : projectile list) (keep : projectile list)
          (hits : hit_event list) (impact_sparks : spark list) =
          match ps with
          | [] -> keep, hits, impact_sparks
          | p :: tl ->
              let x = p.x +. p.vx *. dt in
              let y = p.y +. p.vy *. dt in
              let ttl = p.ttl - 1 in
              let out =
                x < -40.0 || x > float_of_int width +. 40.0 || y < -40.0
                || y > float_of_int height +. 40.0
              in
              if ttl <= 0 || out then step_projectiles tl keep hits impact_sparks
              else
                let hit_target =
                  let found = ref None in
                  for i = 0 to Array.length mages - 1 do
                    let m = mages.(i) in
                    if m.alive && m.team <> p.team && !found = None then (
                      let dx = m.x -. x in
                      let dy = m.y -. y in
                      let rr = (mage_radius +. p.radius) *. (mage_radius +. p.radius) in
                      if (dx *. dx) +. (dy *. dy) <= rr then found := Some m)
                  done;
                  !found
                in
                match hit_target with
                | None ->
                    let p' = { p with x; y; ttl } in
                    step_projectiles tl (p' :: keep) hits impact_sparks
                | Some m ->
                    let ndx, ndy = norm p.vx p.vy in
                    let h =
                      { target = m.id
                      ; source = p.owner
                      ; damage = p.damage
                      ; stun = p.stun
                      ; knockx = ndx *. 0.85
                      ; knocky = ndy *. 0.85
                      }
                    in
                    let sp = sparks_from_hit p.team x y in
                    step_projectiles tl keep (h :: hits) (List.rev_append sp impact_sparks)
        in
        let projectiles_pre = List.rev_append spawned projectiles in
        let projectiles_next, hits, impact_sparks =
          step_projectiles projectiles_pre [] [] []
        in
        let sparks_seed = List.rev_append cast_sparks (List.rev_append impact_sparks sparks) in
        let sparks_next =
          List.filter_map
            (fun s ->
               let ttl = s.ttl - 1 in
               if ttl <= 0 then None
               else
                 Some
                   { s with
                     x = s.x +. s.vx
                   ; y = s.y +. s.vy
                   ; vx = s.vx *. 0.95
                   ; vy = (s.vy *. 0.95) +. 0.03
                   ; ttl
                   })
            sparks_seed
        in
        emit hit_sig hits;
        emit projectile_sig { projectiles = projectiles_next; sparks = sparks_next };
        loop next_id projectiles_next sparks_next
      in
      loop 0 [] [])

let render_driver stop tick_sig output_signal (mages_ref : mage_state array ref)
  (projectile_ref : projectile_frame ref) (pulse_ref : int ref)
  (aggression_ref : float ref) =
  watch stop (fun () ->
      let rec loop () =
        let _ = await tick_sig in
        let mages = !mages_ref in
        let proj = !projectile_ref in
        let ember_alive, azure_alive =
          Array.fold_left
            (fun (e, a) m ->
               if not m.alive then e, a
               else
                 match m.team with
                 | Ember -> e + 1, a
                 | Azure -> e, a + 1)
            (0, 0) mages
        in
        emit output_signal
          { mages
          ; projectiles = proj.projectiles
          ; sparks = proj.sparks
          ; pulse_left = !pulse_ref
          ; aggression = !aggression_ref
          ; ember_alive
          ; azure_alive
          };
        loop ()
      in
      loop ())

let draw_hp_bar renderer x y hp mana =
  let w = 30 in
  let hpw = int_of_float (clamp 0.0 1.0 (hp /. 100.0) *. float_of_int w) in
  let mw = int_of_float (clamp 0.0 1.0 mana *. float_of_int w) in
  ignore (Sdl.set_render_draw_color renderer 24 26 38 210);
  ignore
    (Sdl.render_fill_rect renderer
       (Some (Sdl.Rect.create ~x:(x - (w / 2)) ~y:(y + 18) ~w ~h:4)));
  ignore (Sdl.set_render_draw_color renderer 195 70 78 220);
  ignore
    (Sdl.render_fill_rect renderer
       (Some (Sdl.Rect.create ~x:(x - (w / 2)) ~y:(y + 18) ~w:hpw ~h:4)));
  ignore (Sdl.set_render_draw_color renderer 70 170 230 220);
  ignore
    (Sdl.render_fill_rect renderer
       (Some (Sdl.Rect.create ~x:(x - (w / 2)) ~y:(y + 23) ~w:mw ~h:3)))

let draw_frame renderer (frame : frame) =
  draw_background renderer frame.pulse_left;
  ignore (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_add);
  List.iter
    (fun (p : projectile) ->
       let px = int_of_float p.x in
       let py = int_of_float p.y in
       let jitter = p.id land 7 in
       let ndx, ndy = norm p.vx p.vy in
       let tail = if p.kind = Lance then 20 else 13 in
       let base_r, base_g, base_b = spell_palette p.team p.style in
       let rr = min 255 (base_r + jitter) in
       let gg = min 255 (base_g + jitter) in
       let bb = min 255 (base_b + jitter) in
       let tail_alpha = if p.kind = Lance then 110 else 82 in
       ignore (Sdl.set_render_draw_color renderer rr gg bb tail_alpha);
       ignore
         (Sdl.render_draw_line renderer px py
            (px - int_of_float (ndx *. float_of_int tail))
            (py - int_of_float (ndy *. float_of_int tail)));
       let r = if p.kind = Lance then 4 else 5 in
       draw_blob renderer px py (r + if p.style mod 3 = 1 then 1 else 0)
         (rr, gg, bb) 168;
       if p.kind = Lance then draw_blob renderer px py 2 (245, 250, 255) 145)
    frame.projectiles;
  List.iter
    (fun (s : spark) ->
       let a = int_of_float (clamp 0.0 220.0 (float_of_int (s.ttl * 12))) in
       ignore (Sdl.set_render_draw_color renderer s.r s.g s.b a);
       ignore (Sdl.render_draw_point renderer (int_of_float s.x) (int_of_float s.y)))
    frame.sparks;
  ignore (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend);
  Array.iter
    (fun (m : mage_state) ->
       let px = int_of_float m.x in
       let py = int_of_float m.y in
       let tr, tg, tb = team_color m.team in
       let dead_tint = if m.alive then 1.0 else 0.25 in
       let rr = int_of_float (float_of_int tr *. dead_tint) in
       let gg = int_of_float (float_of_int tg *. dead_tint) in
       let bb = int_of_float (float_of_int tb *. dead_tint) in
       draw_blob renderer px (py + 13) 10 (10, 12, 18) 110;
       if m.shield > 1.0 then
         draw_blob renderer px py
           (14 + int_of_float (m.shield /. 12.0))
           (110, 150, 255) (38 + int_of_float (m.shield *. 2.0));
       let robe_w = if m.role = 1 then 11 else 9 in
       fill_rect renderer (px - (robe_w / 2)) (py - 1) robe_w 14
         (int_of_float (float_of_int rr *. 0.72), int_of_float (float_of_int gg *. 0.72), int_of_float (float_of_int bb *. 0.72))
         (138 + if m.flash > 0 then 70 else 0);
       draw_blob renderer px (py - 7) 6 (rr, gg, bb)
         (110 + if m.flash > 0 then 90 else 0);
       draw_blob renderer px (py - 9) 3 (250, 250, 255)
         (85 + if m.flash > 0 then 80 else 0);
       let staff_ang = m.aura +. (float_of_int m.id *. 0.31) in
       let sx = px + int_of_float (10.0 *. cos staff_ang) in
       let sy = py + int_of_float (10.0 *. sin staff_ang) - 4 in
       ignore (Sdl.set_render_draw_color renderer 210 220 245 150);
       ignore (Sdl.render_draw_line renderer px (py - 3) sx sy);
       let sr, sg, sb = spell_palette m.team m.role in
       draw_blob renderer sx sy 2 (sr, sg, sb) 180;
       draw_role_glyph renderer m.role px (py - 18) m.team;
       if m.stun > 0 then
         draw_blob renderer px (py - 18) 3 (255, 220, 120)
           (120 + int_of_float (float_of_int m.stun *. 3.0));
       draw_hp_bar renderer px py m.hp m.mana)
    frame.mages;
  Sdl.render_present renderer

let scenario input_signal output_signal =
  let stop = new_signal () in
  let tick_sig = new_signal () in
  let cast_sig : (cast_event list, cast_event list) agg_signal =
    new_signal_agg ~initial:[] ~combine:combine_batches
  in
  let hit_sig : (hit_event list, hit_event list) agg_signal =
    new_signal_agg ~initial:[] ~combine:combine_batches
  in
  let mage_sig : (mage_state list, mage_state list) agg_signal =
    new_signal_agg ~initial:[] ~combine:combine_batches
  in
  let projectile_sig = new_signal () in
  let pulse_ref = ref 0 in
  let aggression_ref = ref 1.0 in
  let shield_burst_ref = ref 0 in
  let mages_ref = ref (init_mages ()) in
  let casts_ref : cast_event list ref = ref [] in
  let hits_ref : hit_event list ref = ref [] in
  let projectile_ref = ref { projectiles = []; sparks = [] } in
  watch stop (fun () ->
      parallel
        ((fun () ->
           clock stop tick_sig cast_sig hit_sig mage_sig pulse_ref shield_burst_ref)
         :: (fun () ->
              handle_input input_signal stop pulse_ref aggression_ref shield_burst_ref ())
         :: (fun () -> cast_listener stop cast_sig casts_ref)
         :: (fun () -> hit_listener stop hit_sig hits_ref)
         :: (fun () -> mage_listener stop mage_sig mages_ref)
         :: (fun () -> projectile_listener stop projectile_sig projectile_ref)
         :: (fun () ->
              projectile_manager stop tick_sig casts_ref mages_ref projectile_sig
                hit_sig)
         :: (fun () ->
              render_driver stop tick_sig output_signal mages_ref projectile_ref
                pulse_ref aggression_ref)
         :: Array.to_list
              (Array.map
                 (fun mage ->
                    fun () ->
                      emit mage_sig [ mage ];
                      mage_behavior stop tick_sig mage_sig cast_sig hits_ref mages_ref
                        pulse_ref aggression_ref shield_burst_ref mage)
                 !mages_ref)))

let () =
  Random.self_init ();
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) ->
      Sdl.log "SDL init error: %s" e;
      exit 1
  | Ok () -> (
      match
        Sdl.create_window ~w:width ~h:height "Tempo mage arena (SDL)"
          Sdl.Window.windowed
      with
      | Error (`Msg e) ->
          Sdl.log "SDL window creation error: %s" e;
          exit 1
      | Ok window -> (
          match
            Sdl.create_renderer window ~index:(-1) ~flags:Sdl.Renderer.accelerated
          with
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
                      let sc =
                        Sdl.Event.get event Sdl.Event.keyboard_scancode
                      in
                      let kc =
                        Sdl.Event.get event Sdl.Event.keyboard_keycode
                      in
                      if
                        sc = Sdl.Scancode.q || kc = Sdl.K.q
                        || kc = Sdl.K.escape
                      then Some Quit
                      else if kc = Sdl.K.space then Some Pulse
                      else if kc = Sdl.K.up || kc = int_of_char '+' then Some AggroUp
                      else if kc = Sdl.K.down || kc = int_of_char '-' then Some AggroDown
                      else if kc = int_of_char 's' || kc = int_of_char 'S'
                      then Some ShieldBurst
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
                  let fps =
                    (float_of_int !frames *. 1000.0)
                    /. Int32.to_float elapsed
                  in
                  Sdl.set_window_title window
                    (Printf.sprintf
                       "Tempo mage arena (SDL) - %.1f FPS - behaviors %d - Ember %d / Azure %d - aggro %.2f - [Space/Up/Down/S]"
                       fps mages_count frame.ember_alive frame.azure_alive
                       frame.aggression);
                  frames := 0;
                  last_tick := now)
              in
              execute ~input ~output scenario;
              Sdl.destroy_renderer renderer;
              Sdl.destroy_window window;
              Sdl.quit ()))
