type ('emit, 'agg, 'mode) signal_core =
  ('emit, 'agg, 'mode) Tempo_core.signal_core

type 'a signal = 'a Tempo_core.signal

let present_then_else (s : ('emit, 'agg, 'mode) signal_core)
    (then_ : unit -> unit) (else_ : unit -> unit) : unit =
  let seen = ref false in
  let kill = Tempo_low_level.new_kill () in
  let _ =
    Tempo_core.fork (fun () ->
        Tempo_low_level.with_kill kill (fun () ->
            Tempo_core.when_ s (fun () ->
                seen := true;
                then_ ())))
  in
  Tempo_core.pause ();
  if not !seen then else_ ();
  Tempo_low_level.abort_kill kill

let rec pause_n n =
  if n <= 0 then ()
  else (
    Tempo_core.pause ();
    pause_n (n - 1))

let after_n n body =
  if n < 0 then invalid_arg "after_n expects n >= 0";
  pause_n n;
  body ()

let every_n n body =
  if n <= 0 then invalid_arg "every_n expects n > 0";
  let rec loop_every () =
    pause_n n;
    body ();
    loop_every ()
  in
  loop_every ()

let timeout n ~on_timeout body =
  if n < 0 then invalid_arg "timeout expects n >= 0";
  let timeout_signal = Tempo_core.new_signal () in
  Tempo_core.parallel
    [
      (fun () ->
        after_n n (fun () ->
            Tempo_core.emit timeout_signal ();
            on_timeout ()));
      (fun () -> Tempo_core.watch timeout_signal body);
    ]

let cooldown n s body =
  if n < 0 then invalid_arg "cooldown expects n >= 0";
  let rec loop_cooldown armed =
    if armed then (
      pause_n n;
      loop_cooldown false)
    else (
      let _ = Tempo_core.await s in
      body ();
      loop_cooldown true)
  in
  loop_cooldown false

let supervise_until stop body = Tempo_core.watch stop body

let pulse_n n =
  if n <= 0 then invalid_arg "pulse_n expects n > 0";
  let s = Tempo_core.new_signal () in
  let _ =
    Tempo_core.fork (fun () ->
        every_n n (fun () -> Tempo_core.emit s ()))
  in
  s

let rec loop p () =
  p ();
  Tempo_core.pause ();
  loop p ()

let rec idle () =
  Tempo_core.pause ();
  idle ()

let control (toggle : unit signal) (proc : unit -> unit) =
  let rec loop_control running =
    if running then (
      let _ = Tempo_core.await toggle in
      loop_control false)
    else (
      let _ = Tempo_core.await toggle in
      let _ = Tempo_core.fork (fun () -> Tempo_core.watch toggle proc) in
      loop_control true)
  in
  loop_control false

let alternate (toggle : unit signal) (proc_a : unit -> unit)
    (proc_b : unit -> unit) =
  let rec loop_alternate use_a =
    let proc = if use_a then proc_a else proc_b in
    let _ = Tempo_core.fork (fun () -> Tempo_core.watch toggle proc) in
    let _ = Tempo_core.await toggle in
    loop_alternate (not use_a)
  in
  loop_alternate true
