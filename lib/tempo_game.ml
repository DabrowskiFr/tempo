module Scene = Tempo.Scene
module Resource = Tempo.Resource
module Input_map = Tempo.Input_map
module Event_bus = Tempo.Event_bus
module Fixed_step = Tempo.Fixed_step
module Rng = Tempo.Rng
module Netcode = Tempo.Netcode
module Profiler = Tempo.Profiler
module Tick_tags = Tempo.Tick_tags
module Runtime_snapshot = Tempo.Runtime_snapshot
module Entity_set = Tempo.Entity_set
module Dev_hud = Tempo.Dev_hud
module Error_bus = Tempo.Error_bus
module Timeline_json = Tempo.Timeline_json
module App = Tempo.App
module Loop = Tempo.Loop

module Sync = struct
  type 'evt bus = 'evt Event_bus.channel

  let bus () = Event_bus.channel ()
  let publish = Event_bus.publish
  let await_batch = Event_bus.await_batch

  let rec pump_input decode input_signal bus =
    (match Tempo.peek input_signal with
    | Some raw -> List.iter (publish bus) (decode raw)
    | None -> ());
    Tempo.pause ();
    pump_input decode input_signal bus

  let reducer_loop bus ~init ~step ~on_state =
    let rec loop state =
      let evs = await_batch bus in
      let state' = step state evs in
      on_state state';
      loop state'
    in
    loop init

  let every_n = Tempo.Game.every_n
  let after_n = Tempo.Game.after_n
  let timeout = Tempo.Game.timeout
  let cooldown = Tempo.Game.cooldown
end

module Ui = struct
  type rect = { x : float; y : float; w : float; h : float }
  type pointer = { x : float; y : float }

  type interaction = {
    pointer : pointer;
    down : bool;
    pressed : bool;
  }

  type button = {
    id : string;
    rect : rect;
    label : string;
    enabled : bool;
  }

  let button ~id rect ~label ?(enabled = true) () = { id; rect; label; enabled }

  let contains (r : rect) (p : pointer) =
    p.x >= r.x && p.x <= r.x +. r.w && p.y >= r.y && p.y <= r.y +. r.h

  let button_pressed i b = b.enabled && i.pressed && contains b.rect i.pointer

  type int_stepper = {
    id : string;
    dec_rect : rect;
    inc_rect : rect;
    value : int;
    min_value : int;
    max_value : int;
    step : int;
  }

  let stepper_int ~id rect ~value ~min_value ~max_value ~step =
    let half = rect.w /. 2.0 in
    {
      id;
      dec_rect = { x = rect.x; y = rect.y; w = half; h = rect.h };
      inc_rect = { x = rect.x +. half; y = rect.y; w = half; h = rect.h };
      value;
      min_value;
      max_value;
      step;
    }

  let stepper_delta i s =
    if i.pressed && contains s.dec_rect i.pointer then -s.step
    else if i.pressed && contains s.inc_rect i.pointer then s.step
    else 0

  type slider = {
    id : string;
    rect : rect;
    t : float;
  }

  let slider_v ~id ~rect ~t =
    let t = if t < 0.0 then 0.0 else if t > 1.0 then 1.0 else t in
    { id; rect; t }

  let slider_set i s =
    if i.down && contains s.rect i.pointer then
      let local = (i.pointer.y -. s.rect.y) /. s.rect.h in
      let t = 1.0 -. local in
      let t = if t < 0.0 then 0.0 else if t > 1.0 then 1.0 else t in
      Some t
    else None
end

module Hud = struct
  type panel = {
    rect : Ui.rect;
    title : string;
  }

  type badge = {
    label : string;
    value : string;
  }

  type bar = {
    label : string;
    value : float;
    max_value : float;
  }

  type timer = {
    seconds_left : int;
  }

  let panel ~rect ~title = { rect; title }
  let badge ~label ~value = { label; value }
  let bar ~label ~value ~max_value = { label; value; max_value }
  let timer ~seconds_left = { seconds_left = max 0 seconds_left }

  let timer_text t =
    let m = t.seconds_left / 60 in
    let s = t.seconds_left mod 60 in
    Printf.sprintf "%02d:%02d" m s
end

module Fx = struct
  type kind =
    | Success
    | Warn
    | Error_kind
    | Info

  type toast = {
    text : string;
    kind : kind;
    ttl : int;
  }

  type floating_text = {
    text : string;
    x : float;
    y : float;
    kind : kind;
    ttl : int;
  }

  type screen_pulse = {
    kind : kind;
    ttl : int;
  }

  type item =
    | Toast of toast
    | Floating_text of floating_text
    | Screen_pulse of screen_pulse

  type t = item list

  let empty = []
  let add st fx = fx :: st
  let toast ~ttl ~text ~kind = Toast { ttl; text; kind }
  let floating_text ~ttl ~text ~x ~y ~kind = Floating_text { ttl; text; x; y; kind }
  let screen_pulse ~ttl ~kind = Screen_pulse { ttl; kind }

  let update_one = function
    | Toast t when t.ttl > 1 -> Some (Toast { t with ttl = t.ttl - 1 })
    | Floating_text t when t.ttl > 1 ->
        Some (Floating_text { t with ttl = t.ttl - 1; y = t.y -. 1.0 })
    | Screen_pulse p when p.ttl > 1 -> Some (Screen_pulse { p with ttl = p.ttl - 1 })
    | _ -> None

  let update st = List.filter_map update_one st
  let active st = List.rev st
end

module Audio = struct
  type cue = {
    freq_hz : float;
    duration_s : float;
    volume : float;
  }

  type music_mode =
    | Silent
    | Patrol
    | Tension
    | Results

  type policy_state = {
    started : bool;
    paused : bool;
    game_over : bool;
    tension_count : int;
  }

  let cue ~freq_hz ~duration_s ~volume = { freq_hz; duration_s; volume }
  let play_on_event route ev = route ev

  let music_policy s =
    if not s.started || s.paused then Silent
    else if s.game_over then Results
    else if s.tension_count > 0 then Tension
    else Patrol
end

module Scene_view = struct
  type scene =
    | Menu
    | Game
    | Pause
    | Results
    | Custom of string

  type overlay =
    | Dim
    | Modal of {
        title : string;
        body : string;
      }

  type cmd =
    | Set_scene of scene
    | Push_overlay of overlay
    | Pop_overlay

  type t = {
    scene : scene;
    overlays : overlay list;
  }

  let initial scene = { scene; overlays = [] }

  let transition _from to_ =
    match to_ with
    | Pause -> [ Push_overlay Dim; Set_scene Pause ]
    | Results -> [ Push_overlay (Modal { title = "Results"; body = "" }); Set_scene Results ]
    | _ -> [ Set_scene to_ ]

  let apply st = function
    | Set_scene scene -> { st with scene }
    | Push_overlay ov -> { st with overlays = ov :: st.overlays }
    | Pop_overlay ->
        (match st.overlays with
        | [] -> st
        | _ :: tl -> { st with overlays = tl })

  let apply_all st cmds = List.fold_left apply st cmds
end

module Integration = struct
  module type BACKEND = sig
    type raw_input
    type output_cmd

    val read : unit -> raw_input
    val write : output_cmd list -> unit
  end

  type ('raw, 'evt) input_adapter = 'raw -> 'evt list
  type ('state, 'cmd) projector = 'state -> 'cmd list

  let rec run_read_phase ~read ~decode bus =
    List.iter (Sync.publish bus) (decode (read ()));
    Tempo.pause ();
    run_read_phase ~read ~decode bus

  let run_sync_phase bus ~init ~step ~on_state = Sync.reducer_loop bus ~init ~step ~on_state

  let make_write_phase ~project ~write state = write (project state)
end

module Store = struct
  type ('model, 'msg, 'view) app = {
    init : 'model;
    update : 'model -> 'msg -> 'model;
    view : 'model -> 'view;
    equal_view : 'view -> 'view -> bool;
  }

  let run ?instants ?(input = fun () -> None) ?(output = fun _ -> ()) app =
    let model = ref app.init in
    let last_view : 'view option ref = ref None in
    Tempo.execute ?instants ~input ~output (fun input_signal output_signal ->
        let rec loop () =
          (match Tempo.peek input_signal with
          | Some msg -> model := app.update !model msg
          | None -> ());
          let v = app.view !model in
          (match !last_view with
          | Some prev when app.equal_view prev v -> ()
          | _ ->
              last_view := Some v;
              Tempo.emit output_signal v);
          Tempo.pause ();
          loop ()
        in
        loop ())
end
