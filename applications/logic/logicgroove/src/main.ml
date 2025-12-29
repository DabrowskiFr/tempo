open Tempo
open Raylib

type mode = Monitor | Assist | Strict

type input = {
  toggle_run : bool;
  cycle_mode : bool;
  randomize : bool;
  apply_fix : bool;
}

type pattern = {
  kick : bool array;
  snare : bool array;
  hat : bool array;
}

type suggestion = { text : string; step : int; cost : int }

type state = {
  pat : pattern;
  step : int;
  running : bool;
  mode : mode;
  verdict : string;
  violations : int;
  suggestions : suggestion list;
}

let steps = 16
let width = 1020
let height = 680

let mode_to_string = function Monitor -> "Monitor" | Assist -> "Assist" | Strict -> "Strict"
let next_mode = function Monitor -> Assist | Assist -> Strict | Strict -> Monitor

let random_pattern rng =
  let mk p = Array.init steps (fun _ -> Rng.float rng 1.0 < p) in
  { kick = mk 0.35; snare = mk 0.22; hat = mk 0.58 }

let copy_arr a = Array.init (Array.length a) (fun i -> a.(i))
let copy_pat p = { kick = copy_arr p.kick; snare = copy_arr p.snare; hat = copy_arr p.hat }

let has_snare_within pat from k =
  let rec loop i =
    if i > k then false
    else if pat.snare.((from + i) mod steps) then true
    else loop (i + 1)
  in
  loop 0

let first_free_snare_step pat from k =
  let rec loop i =
    if i > k then None
    else
      let s = (from + i) mod steps in
      if pat.snare.(s) then loop (i + 1) else Some s
  in
  loop 0

let eval_rules pat step =
  let r1_ok = if pat.kick.(step) then has_snare_within pat step 8 else true in
  let r2_ok = if pat.snare.(step) then not pat.snare.((step + 1) mod steps) else true in
  (r1_ok && r2_ok, r1_ok, r2_ok)

let mk_suggestion pat step =
  if pat.kick.(step) then
    match first_free_snare_step pat step 8 with
    | Some s -> Some { text = Printf.sprintf "Add SNARE at step %d" s; step = s; cost = 1 }
    | None -> None
  else None

let poll_input () =
  if window_should_close () || is_key_pressed Key.Escape then raise Exit;
  Some
    {
      toggle_run = is_key_pressed Key.Space;
      cycle_mode = is_key_pressed Key.Tab;
      randomize = is_key_pressed Key.R;
      apply_fix = is_key_pressed Key.A;
    }

let draw_row title y arr step c =
  draw_text title 32 y 24 Color.raywhite;
  for i = 0 to steps - 1 do
    let x = 220 + (i * 42) in
    draw_rectangle x y 34 28 (if arr.(i) then c else Color.create 44 49 57 255);
    if i = step then draw_rectangle_lines (x - 2) (y - 2) 38 32 Color.gold
  done

let render s =
  begin_drawing ();
  clear_background (Color.create 14 18 25 255);
  draw_text "LogicGroove (LTL Monitor + Repair)" 24 18 34 Color.raywhite;
  draw_text "SPACE run/pause | TAB mode | A apply fix | R randomize" 24 54 20 (Color.create 189 215 236 255);
  draw_row "KICK" 128 s.pat.kick s.step (Color.create 242 130 65 255);
  draw_row "SNARE" 194 s.pat.snare s.step (Color.create 95 170 255 255);
  draw_row "HAT" 260 s.pat.hat s.step (Color.create 220 209 109 255);

  draw_text (Printf.sprintf "Mode: %s" (mode_to_string s.mode)) 24 340 26 Color.raywhite;
  draw_text (Printf.sprintf "Verdict: %s" s.verdict) 24 374 24 (if s.verdict = "False" then Color.red else if s.verdict = "True" then Color.lime else Color.orange);
  draw_text (Printf.sprintf "Violations: %d" s.violations) 24 406 24 Color.gold;

  draw_text "Suggestions:" 24 454 24 Color.raywhite;
  List.iteri
    (fun i sug ->
      draw_text (Printf.sprintf "%d. %s (cost %d)" (i + 1) sug.text sug.cost) 40 (486 + (i * 24)) 20 (Color.create 208 233 255 255))
    s.suggestions;

  end_drawing ()

let main input output =
  let rng = Rng.create 8080 in
  let state =
    new_state
      {
        pat = random_pattern rng;
        step = 0;
        running = true;
        mode = Monitor;
        verdict = "?";
        violations = 0;
        suggestions = [];
      }
  in
  let repairs = Event_bus.channel () in

  let apply_suggestion (sug : suggestion) =
    modify_state state (fun s ->
        let p = copy_pat s.pat in
        p.snare.(sug.step) <- true;
        { s with pat = p; suggestions = [] })
  in

  let input_proc () =
    let rec loop () =
      let i = await input in
      if i.toggle_run then modify_state state (fun s -> { s with running = not s.running });
      if i.cycle_mode then modify_state state (fun s -> { s with mode = next_mode s.mode });
      if i.randomize then modify_state state (fun s -> { s with pat = random_pattern rng; violations = 0; verdict = "?"; suggestions = [] });
      if i.apply_fix then
        match (get_state state).suggestions with
        | sug :: _ -> apply_suggestion sug
        | [] -> ();
      loop ()
    in
    loop ()
  in

  let sequencer () =
    Game.every_n 4 (fun () ->
        let s = get_state state in
        if s.running then (
          let step = (s.step + 1) mod steps in
          let ok, r1_ok, r2_ok = eval_rules s.pat step in
          let suggestions = if ok then [] else (match mk_suggestion s.pat step with Some x -> [ x ] | None -> []) in
          let verdict = if ok then "?" else "False" in
          let violations = if ok then s.violations else s.violations + 1 in
          set_state state { s with step; verdict; violations; suggestions };
          if not r1_ok then Event_bus.publish repairs "NeedSnare";
          if not r2_ok then Event_bus.publish repairs "NoDoubleSnare"))
  in

  let repair_engine () =
    let rec loop () =
      let _events = Event_bus.await_batch repairs in
      let s = get_state state in
      match (s.mode, s.suggestions) with
      | Strict, sug :: _ -> apply_suggestion sug
      | _ -> ();
      loop ()
    in
    loop ()
  in

  let renderer () =
    let rec loop () =
      emit output (get_state state);
      pause ();
      loop ()
    in
    loop ()
  in

  parallel [ input_proc; sequencer; repair_engine; renderer ]

let () =
  init_window width height "LogicGroove";
  set_target_fps 60;
  (try execute ~input:poll_input ~output:render main with Exit -> ());
  close_window ()
