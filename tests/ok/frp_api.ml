open Tempo

let fail msg = failwith msg

let test_map_filter_fold () =
  let src = new_signal () in
  let mapped = Frp.map (fun x -> x + 1) src in
  let even = Frp.filter (fun x -> x mod 2 = 0) mapped in
  let sum = Frp.fold ~initial:0 ( + ) even in
  let producer () =
    emit src 0;
    pause ();
    emit src 1;
    pause ();
    emit src 2;
    pause ();
    emit src 3
  in
  let consumer () =
    let a = await even in
    if a <> 2 then fail "map/filter first mismatch";
    let b = await even in
    if b <> 4 then fail "map/filter second mismatch";
    pause ();
    if get_state sum <> 6 then fail "fold mismatch"
  in
  parallel [ producer; consumer ]

let test_throttle () =
  let src = new_signal () in
  let throttled = Frp.throttle_n 2 src in
  let producer () =
    emit src 10;
    pause ();
    emit src 11;
    pause ();
    emit src 12;
    pause ();
    emit src 13
  in
  let consumer () =
    let a = await throttled in
    if a <> 10 then fail "throttle first mismatch";
    let b = await throttled in
    if b <> 13 then fail "throttle second mismatch"
  in
  parallel [ producer; consumer ]

let test_once_edge () =
  let b = new_signal () in
  let edges = Frp.edge b in
  let first = Frp.once edges in
  let count = new_state 0 in
  let producer () =
    emit b false;
    pause ();
    emit b true;
    pause ();
    emit b true;
    pause ();
    emit b false;
    pause ();
    emit b true
  in
  let consumer () =
    let _ = await first in
    modify_state count (fun x -> x + 1);
    Game.after_n 4 (fun () -> if get_state count <> 1 then fail "once should emit once")
  in
  parallel [ producer; consumer ]

let test_switch_once () =
  let trigger = new_signal () in
  let st = new_state 0 in
  let switcher () =
    Frp.switch_once trigger (fun v () ->
        modify_state st (fun x -> x + v);
        let rec loop () =
          pause ();
          modify_state st (fun x -> x + 1);
          loop ()
        in
        loop ())
  in
  let producer () =
    pause ();
    emit trigger 5;
    pause ();
    emit trigger 9
  in
  let checker () =
    Game.after_n 6 (fun () ->
        let v = get_state st in
        if v < 8 || v > 12 then fail "switch_once range mismatch")
  in
  parallel [ switcher; producer; checker ]

let () =
  execute ~instants:40 (fun _ _ ->
      test_map_filter_fold ();
      test_throttle ();
      test_once_edge ();
      test_switch_once ())
