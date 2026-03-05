open Tempo_core_api

let present_then_else (s : ('emit, 'agg, 'mode) Tempo_types.signal_core)
    (then_ : unit -> unit) (else_ : unit -> unit) : unit =
  let seen = ref false in
  let kill = Tempo_low_level.new_kill () in
  let _ =
    Tempo_low_level.fork (fun () ->
        Tempo_low_level.with_kill kill (fun () ->
            Tempo_low_level.with_guard s (fun () ->
                seen := true;
                then_ ())))
  in
  pause ();
  if not !seen then else_ ();
  Tempo_low_level.abort_kill kill

let rec pause_n n =
  if n <= 0 then ()
  else (
    pause ();
    pause_n (n - 1))

let after_n n body =
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
  let timeout_signal = new_signal () in
  parallel
    [
      (fun () ->
        after_n n (fun () ->
            emit timeout_signal ();
            on_timeout ()));
      (fun () -> watch timeout_signal body);
    ]

let cooldown n s body =
  let rec loop_cooldown armed =
    if armed then (
      pause_n n;
      loop_cooldown false)
    else (
      await s |> ignore;
      body ();
      loop_cooldown true)
  in
  loop_cooldown false

let rising_edge s =
  let prev = ref false in
  loop
    (fun () ->
      let current = Option.value ~default:false (Tempo_low_level.peek s) in
      if (not !prev) && current then ()
      else pause ();
      prev := current)
    ()

let falling_edge s =
  let prev = ref false in
  loop
    (fun () ->
      let current = Option.value ~default:false (Tempo_low_level.peek s) in
      if !prev && not current then ()
      else pause ();
      prev := current)
    ()

let edge_by eq s =
  let first = ref true in
  let prev = ref None in
  loop
    (fun () ->
      let current = Tempo_low_level.peek s in
      match current with
      | None -> pause ()
      | Some value ->
          let changed =
            if !first then false
            else
              match !prev with
              | Some old -> not (eq old value)
              | None -> true
          in
          prev := Some value;
          first := false;
          if changed then () else pause ())
    ()

let pulse_n n =
  let s = new_signal () in
  let _ =
    Tempo_low_level.fork (fun () ->
        every_n n (fun () -> emit s ()))
  in
  s

let supervise_until stop body = watch stop body
