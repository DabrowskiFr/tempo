open Tempo
open Tempo_frp

let fail msg = failwith msg

let test_map_filter () =
  let src = new_signal () in
  let mapped = Frp.map (fun x -> x + 1) src in
  let even = Frp.filter (fun x -> x mod 2 = 0) mapped in
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
    if b <> 4 then fail "map/filter second mismatch"
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
  let seen = new_signal () in
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
    emit seen ();
    Game.after_n 4 (fun () ->
        when_ seen (fun () -> ());
        let rec wait_second () =
          when_ seen (fun () -> fail "once should emit once");
          pause ();
          wait_second ()
        in
        wait_second ())
  in
  parallel [ producer; consumer ]

let test_switch_once () =
  let trigger = new_signal () in
  let ticks = new_signal () in
  let switcher () =
    Frp.switch_once trigger (fun v () ->
        emit ticks v;
        let rec loop () =
          pause ();
          emit ticks 1;
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
    let total = ref 0 in
    let rec collect n =
      if n <= 0 then (
        if !total < 8 || !total > 12 then fail "switch_once range mismatch")
      else
        let v = await ticks in
        total := !total + v;
        collect (n - 1)
    in
    collect 4
  in
  parallel [ switcher; producer; checker ]

let () =
  execute ~instants:40 (fun _ _ ->
      test_map_filter ();
      test_throttle ();
      test_once_edge ();
      test_switch_once ())
