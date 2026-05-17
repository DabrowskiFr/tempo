open Tempo

let observe_guard ~guard ~events ~hits () =
  let rec loop seen remaining =
    if remaining = 0 then ()
    else (
      when_ guard (fun () ->
          incr hits);
      pause ();
      loop (seen + 1) (remaining - 1))
  in
  loop 0 events

let combined_listener guard_a guard_b combo_hits combo_done () =
  when_ guard_a (fun () ->
      when_ guard_b (fun () ->
          incr combo_hits));
  pause ();
  combo_done := true

let driver guard_a guard_b () =
  emit guard_a ();
  pause ();
  emit guard_a ();
  emit guard_b ();
  pause ();
  emit guard_a ();
  pause ()

let scenario () =
  let guard_a = new_signal () in
  let guard_b = new_signal () in
  let a_hits = ref 0 in
  let b_hits = ref 0 in
  let combo_hits = ref 0 in
  let combo_done = ref false in
  parallel
    [
      driver guard_a guard_b
    ; observe_guard ~guard:guard_a ~events:3 ~hits:a_hits
    ; observe_guard ~guard:guard_b ~events:1 ~hits:b_hits
    ; combined_listener guard_a guard_b combo_hits combo_done
    ];
  if !a_hits <> 3 then failwith "listener A did not observe 3 activations";
  if !b_hits <> 1 then failwith "listener B did not observe 1 activation";
  if !combo_hits <> 1 then failwith "combined listener did not trigger exactly once";
  if not !combo_done then failwith "combined listener did not finish";
  Format.printf "listener_a_hits=%d@.%!" !a_hits;
  Format.printf "listener_b_hits=%d@.%!" !b_hits;
  Format.printf "combo_hits=%d@.%!" !combo_hits;
  Format.printf "combo_done=%b@.%!" !combo_done

let () = execute (fun _ _ -> scenario ())
