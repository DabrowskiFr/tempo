module App = struct
  type 'msg dispatch = 'msg -> unit
  type 'msg command = dispatch:'msg dispatch -> unit

  let none ~dispatch:_ = ()
  let emit msg ~dispatch = dispatch msg

  let after_n n msg ~dispatch =
    if n < 0 then invalid_arg "Tempo_app.App.after_n: n must be >= 0";
    ignore (Tempo.Low_level.fork (fun () -> Tempo.Constructs.after_n n (fun () -> dispatch msg)))

  let every_n n msg ~dispatch =
    if n <= 0 then invalid_arg "Tempo_app.App.every_n: n must be > 0";
    ignore (Tempo.Low_level.fork (fun () -> Tempo.Constructs.every_n n (fun () -> dispatch msg)))

  let tick_every n ~tick = every_n n tick
  let tick_if enabled n ~tick = if enabled then tick_every n ~tick else none
  let command_if cond cmd = if cond then cmd else none
  let command_when cond ~then_ ~else_ = if cond then then_ else else_
  let batch cmds ~dispatch = List.iter (fun cmd -> cmd ~dispatch) cmds
  let present_then_else = Tempo.Constructs.present_then_else

  let boot_once_input ~boot input =
    let boot_sent = ref false in
    fun () ->
      if not !boot_sent then (
        boot_sent := true;
        Some boot)
      else input ()

  let input_union inputs () =
    let rec pick = function
      | [] -> None
      | f :: fs -> (match f () with Some _ as m -> m | None -> pick fs)
    in
    pick inputs

  let with_boot_and_tick ~boot ~tick ~tick_every:n ~input =
    if n <= 0 then invalid_arg "Tempo_app.App.with_boot_and_tick: tick_every must be > 0";
    (boot_once_input ~boot input, tick_every n ~tick)

  type ('model, 'msg) program = {
    init : 'model;
    update : 'model -> 'msg -> 'model * 'msg command;
  }

  let run ?instants ?(input = fun () -> None) ?(on_model = fun _ -> ()) (program : ('model, 'msg) program) =
    Tempo.execute ?instants ~input (fun input_signal _ ->
        let model = ref program.init in
        let pending : 'msg Queue.t = Queue.create () in
        let dispatch msg = Queue.push msg pending in
        let rec drain () =
          if not (Queue.is_empty pending) then (
            let msg = Queue.pop pending in
            let m', cmd = program.update !model msg in
            model := m';
            cmd ~dispatch;
            drain ())
        in
        let rec loop () =
          (match Tempo.Low_level.peek input_signal with Some msg -> dispatch msg | None -> ());
          drain ();
          on_model !model;
          Tempo.pause ();
          loop ()
        in
        loop ())

  let run_with_view ?instants ?(input = fun () -> None) ?(equal_view = ( = )) ~view ?(output = fun _ -> ())
      (program : ('model, 'msg) program) =
    Tempo.execute ?instants ~input ~output (fun input_signal output_signal ->
        let model = ref program.init in
        let pending : 'msg Queue.t = Queue.create () in
        let last_view : 'view option ref = ref None in
        let dispatch msg = Queue.push msg pending in
        let rec drain () =
          if not (Queue.is_empty pending) then (
            let msg = Queue.pop pending in
            let m', cmd = program.update !model msg in
            model := m';
            cmd ~dispatch;
            drain ())
        in
        let maybe_emit_view () =
          let v = view !model in
          match !last_view with
          | Some prev when equal_view prev v -> ()
          | _ ->
              last_view := Some v;
              Tempo.emit output_signal v
        in
        let rec loop () =
          (match Tempo.Low_level.peek input_signal with Some msg -> dispatch msg | None -> ());
          drain ();
          maybe_emit_view ();
          Tempo.pause ();
          loop ()
        in
        loop ())
end

module Loop = struct
  type ('input, 'output, 'state) config = {
    init : 'state;
    input : unit -> 'input option;
    step : 'state -> 'input option -> 'state * 'output option;
    output : 'output -> unit;
  }

  let run ?instants cfg =
    let state = ref cfg.init in
    let main input_signal output_signal =
      let rec loop () =
        let seen = ref None in
        Tempo.when_ input_signal (fun () -> seen := Some (Tempo.await_immediate input_signal));
        let next_state, out = cfg.step !state !seen in
        state := next_state;
        (match out with Some v -> Tempo.emit output_signal v | None -> ());
        Tempo.pause ();
        loop ()
      in
      loop ()
    in
    Tempo.execute ?instants ~input:cfg.input ~output:cfg.output main
end

module Scene = struct
  type 'id t = {
    request_sig : 'id Tempo.signal;
    mutable current_id : 'id option;
    equal : 'id -> 'id -> bool;
    on_enter : 'id -> unit;
    on_exit : 'id -> unit;
  }

  let create ?(equal = ( = )) ~on_enter ~on_exit () =
    { request_sig = Tempo.new_signal (); current_id = None; equal; on_enter; on_exit }

  let request t id = Tempo.emit t.request_sig id
  let current t = t.current_id

  let process t =
    let rec loop () =
      let next = Tempo.await t.request_sig in
      (match t.current_id with
      | Some prev when t.equal prev next -> ()
      | Some prev ->
          t.on_exit prev;
          t.current_id <- Some next;
          t.on_enter next
      | None ->
          t.current_id <- Some next;
          t.on_enter next);
      loop ()
    in
    loop ()
end
