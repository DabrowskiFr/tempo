open Tempo
open Raylib

type sid = S0 | S1 | S2

type ltl =
  | True
  | False
  | Prop of string
  | Not of ltl
  | And of ltl * ltl
  | Or of ltl * ltl
  | X of ltl
  | F of ltl
  | G of ltl
  | U of ltl * ltl

type verdict = VTrue | VFalse | VInconclusive

type tick_info = {
  tick : int;
  st : sid;
  props : string list;
}

type input = {
  toggle_run : bool;
  step : bool;
  reset : bool;
  cycle_formula : bool;
}

type sim = {
  current : sid;
  tick : int;
  trace : tick_info list;
  running : bool;
  formula_name : string;
  residual : ltl;
  verdict : verdict;
}

let width = 1180
let height = 720

let sid_to_string = function S0 -> "S0" | S1 -> "S1" | S2 -> "S2"

let string_of_ltl =
  let rec go = function
    | True -> "True"
    | False -> "False"
    | Prop p -> p
    | Not a -> "!" ^ go a
    | And (a, b) -> "(" ^ go a ^ " & " ^ go b ^ ")"
    | Or (a, b) -> "(" ^ go a ^ " | " ^ go b ^ ")"
    | X a -> "X " ^ go a
    | F a -> "F " ^ go a
    | G a -> "G " ^ go a
    | U (a, b) -> "(" ^ go a ^ " U " ^ go b ^ ")"
  in
  go

let simplify =
  let rec s = function
    | And (True, x) | And (x, True) -> s x
    | And (False, _) | And (_, False) -> False
    | Or (False, x) | Or (x, False) -> s x
    | Or (True, _) | Or (_, True) -> True
    | Not True -> False
    | Not False -> True
    | Not (Not x) -> s x
    | And (a, b) -> And (s a, s b)
    | Or (a, b) -> Or (s a, s b)
    | Not a -> Not (s a)
    | X a -> X (s a)
    | F a -> F (s a)
    | G a -> G (s a)
    | U (a, b) -> U (s a, s b)
    | x -> x
  in
  s

let progress valuation =
  let rec p = function
    | True -> True
    | False -> False
    | Prop x -> if valuation x then True else False
    | Not a -> simplify (Not (p a))
    | And (a, b) -> simplify (And (p a, p b))
    | Or (a, b) -> simplify (Or (p a, p b))
    | X a -> a
    | F a -> simplify (Or (p a, F a))
    | G a -> simplify (And (p a, G a))
    | U (a, b) -> simplify (Or (p b, And (p a, U (a, b))))
  in
  p

let verdict_of = function True -> VTrue | False -> VFalse | _ -> VInconclusive

let formulas =
  [ ("P1: G (p -> F q)", G (Or (Not (Prop "p"), F (Prop "q"))))
  ; ("P2: F (r & X p)", F (And (Prop "r", X (Prop "p"))))
  ; ("P3: G (q -> X !q)", G (Or (Not (Prop "q"), X (Not (Prop "q")))))
  ]

let props_of_sid = function
  | S0 -> [ "p" ]
  | S1 -> [ "q" ]
  | S2 -> [ "p"; "r" ]

let valuation sid x = List.mem x (props_of_sid sid)

let next_state tick =
  function
  | S0 -> S1
  | S1 -> S2
  | S2 -> if tick mod 3 = 0 then S2 else S0

let poll_input () =
  if window_should_close () || is_key_pressed Key.Escape then raise Exit;
  Some
    {
      toggle_run = is_key_pressed Key.Space;
      step = is_key_pressed Key.N;
      reset = is_key_pressed Key.R;
      cycle_formula = is_key_pressed Key.F;
    }

let render s =
  let verdict_text, verdict_color =
    match s.verdict with
    | VTrue -> ("True", Color.lime)
    | VFalse -> ("False", Color.red)
    | VInconclusive -> ("?", Color.orange)
  in
  begin_drawing ();
  clear_background (Color.create 15 18 24 255);
  draw_text "TemporalSim" 24 18 34 Color.raywhite;
  draw_text "SPACE run/pause | N step | R reset | F cycle formula | ESC quit" 24 56 20 (Color.create 188 214 236 255);

  draw_rectangle_lines 20 96 370 586 (Color.create 75 98 126 255);
  draw_text "Model" 34 108 24 Color.raywhite;
  draw_text "States: S0(p), S1(q), S2(p,r)" 34 140 20 Color.raywhite;
  draw_text "Transitions: S0->S1->S2->(S0|S2)" 34 166 20 Color.raywhite;
  draw_text (Printf.sprintf "Current: %s" (sid_to_string s.current)) 34 198 24 Color.gold;
  draw_text (Printf.sprintf "Tick: %d" s.tick) 34 228 24 Color.skyblue;

  draw_rectangle_lines 406 96 370 586 (Color.create 75 98 126 255);
  draw_text "Trace (latest 16)" 420 108 24 Color.raywhite;
  let recent = List.rev (List.rev s.trace |> List.to_seq |> List.of_seq |> fun l -> if List.length l > 16 then List.rev (List.rev l |> List.tl) else l) in
  List.iteri
    (fun i (t : tick_info) ->
      draw_text
        (Printf.sprintf "%03d | %s | {%s}" t.tick (sid_to_string t.st) (String.concat "," t.props))
        420 (140 + (i * 30)) 20 (Color.create 201 219 236 255))
    recent;

  draw_rectangle_lines 792 96 368 586 (Color.create 75 98 126 255);
  draw_text "LTL Monitor" 806 108 24 Color.raywhite;
  draw_text s.formula_name 806 142 20 Color.gold;
  draw_text (Printf.sprintf "Residual: %s" (string_of_ltl s.residual)) 806 176 20 (Color.create 201 219 236 255);
  draw_text (Printf.sprintf "Verdict: %s" verdict_text) 806 210 26 verdict_color;
  draw_text (if s.running then "RUNNING" else "PAUSED") 806 246 22 (if s.running then Color.lime else Color.orange);
  end_drawing ()

let initial_sim () =
  let name, f = List.hd formulas in
  {
    current = S0;
    tick = 0;
    trace = [ { tick = 0; st = S0; props = props_of_sid S0 } ];
    running = false;
    formula_name = name;
    residual = f;
    verdict = VInconclusive;
  }

let cycle_formula s =
  let idx =
    match List.find_opt (fun (n, _) -> n = s.formula_name) formulas with
    | None -> 0
    | Some _ ->
        let rec find i = function
          | [] -> 0
          | (n, _) :: tl -> if n = s.formula_name then i else find (i + 1) tl
        in
        find 0 formulas
  in
  let nidx = (idx + 1) mod List.length formulas in
  let name, f = List.nth formulas nidx in
  { s with formula_name = name; residual = f; verdict = VInconclusive }

let step_once s =
  let st' = next_state s.tick s.current in
  let tick' = s.tick + 1 in
  let residual' = simplify (progress (valuation st') s.residual) in
  let verdict' = verdict_of residual' in
  {
    s with
    current = st';
    tick = tick';
    trace = ({ tick = tick'; st = st'; props = props_of_sid st' } :: s.trace);
    residual = residual';
    verdict = verdict';
  }

let main input output =
  let sim = State.create (initial_sim ()) in

  let input_proc () =
    let rec loop () =
      let i = await input in
      if i.toggle_run then State.modify sim (fun s -> { s with running = not s.running });
      if i.step then State.modify sim step_once;
      if i.reset then State.set sim (initial_sim ());
      if i.cycle_formula then State.modify sim cycle_formula;
      loop ()
    in
    loop ()
  in

  let run_proc () =
    Game.every_n 8 (fun () -> if (State.get sim).running then State.modify sim step_once)
  in

  let render_proc () =
    let rec loop () =
      emit output (State.get sim);
      pause ();
      loop ()
    in
    loop ()
  in

  parallel [ input_proc; run_proc; render_proc ]

let () =
  init_window width height "TemporalSim";
  set_target_fps 60;
  (try execute ~input:poll_input ~output:render main with Exit -> ());
  close_window ()
