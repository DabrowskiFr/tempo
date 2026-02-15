open Tempo

let fail msg = failwith msg

let test_arr_identity () =
  let src = new_signal () in
  let out = SF.run SF.(arr Fun.id) src in
  let producer () =
    emit src 5;
    pause ();
    emit src 7
  in
  let consumer () =
    let a = await out in
    if a <> 5 then fail "arr identity first mismatch";
    let b = await out in
    if b <> 7 then fail "arr identity second mismatch"
  in
  parallel [ producer; consumer ]

let test_compose () =
  let src = new_signal () in
  let sf = SF.(arr (fun x -> x + 1) >>> arr (fun x -> x * 2)) in
  let out = SF.run sf src in
  let producer () =
    emit src 1;
    pause ();
    emit src 2
  in
  let consumer () =
    let a = await out in
    if a <> 4 then fail "compose first mismatch";
    let b = await out in
    if b <> 6 then fail "compose second mismatch"
  in
  parallel [ producer; consumer ]

let test_compose_associative () =
  let src = new_signal () in
  let f = SF.arr (fun x -> x + 1) in
  let g = SF.arr (fun x -> x * 2) in
  let h = SF.arr (fun x -> x - 3) in
  let lhs = SF.((f >>> g) >>> h) in
  let rhs = SF.(f >>> (g >>> h)) in
  let out = SF.run SF.(lhs &&& rhs) src in
  let producer () =
    emit src 2;
    pause ();
    emit src 8
  in
  let consumer () =
    let a_l, a_r = await out in
    if a_l <> a_r then fail "compose associative first mismatch";
    if a_l <> 3 then fail "compose associative first expected mismatch";
    let b_l, b_r = await out in
    if b_l <> b_r then fail "compose associative second mismatch";
    if b_l <> 15 then fail "compose associative second expected mismatch"
  in
  parallel [ producer; consumer ]

let test_fanout () =
  let src = new_signal () in
  let sf = SF.(arr (fun x -> x + 1) &&& arr (fun x -> x * 3)) in
  let out = SF.run sf src in
  let producer () =
    emit src 2;
    pause ();
    emit src 4
  in
  let consumer () =
    let a = await out in
    if a <> (3, 6) then fail "fanout first mismatch";
    let b = await out in
    if b <> (5, 12) then fail "fanout second mismatch"
  in
  parallel [ producer; consumer ]

let test_hold () =
  let src = new_signal () in
  let sf_events =
    SF.arr (fun x -> if x = 0 then SF.Event.NoEvent else SF.Event.Event x)
  in
  let held = SF.(hold ~initial:0 sf_events) in
  let out = SF.run held src in
  let producer () =
    emit src 0;
    pause ();
    emit src 3;
    pause ();
    emit src 0;
    pause ();
    emit src 4
  in
  let consumer () =
    let a = await out in
    if a <> 0 then fail "hold first mismatch";
    let b = await out in
    if b <> 3 then fail "hold second mismatch";
    let c = await out in
    if c <> 3 then fail "hold third mismatch";
    let d = await out in
    if d <> 4 then fail "hold fourth mismatch"
  in
  parallel [ producer; consumer ]

let test_edge () =
  let src = new_signal () in
  let sf = SF.(edge (arr Fun.id)) in
  let out = SF.run sf src in
  let producer () =
    emit src false;
    pause ();
    emit src true;
    pause ();
    emit src true;
    pause ();
    emit src false;
    pause ();
    emit src true
  in
  let consumer () =
    let expect = function
      | SF.Event.NoEvent -> ()
      | SF.Event.Event () -> fail "edge expected NoEvent"
    in
    expect (await out);
    (match await out with
    | SF.Event.Event () -> ()
    | SF.Event.NoEvent -> fail "edge expected first Event");
    expect (await out);
    expect (await out);
    (match await out with
    | SF.Event.Event () -> ()
    | SF.Event.NoEvent -> fail "edge expected second Event")
  in
  parallel [ producer; consumer ]

let test_switch () =
  let src = new_signal () in
  let sf0 =
    SF.arr (fun x ->
        (x + 1, if x >= 3 then SF.Event.Event 10 else SF.Event.NoEvent))
  in
  let sf = SF.switch sf0 (fun delta -> SF.arr (fun x -> x + delta)) in
  let out = SF.run sf src in
  let producer () =
    emit src 1;
    pause ();
    emit src 2;
    pause ();
    emit src 3;
    pause ();
    emit src 4
  in
  let consumer () =
    let a = await out in
    if a <> 2 then fail "switch first mismatch";
    let b = await out in
    if b <> 3 then fail "switch second mismatch";
    let c = await out in
    if c <> 4 then fail "switch third mismatch";
    let d = await out in
    if d <> 14 then fail "switch fourth mismatch"
  in
  parallel [ producer; consumer ]

let test_integral () =
  let src = new_signal () in
  let sf = SF.integral ~dt:0.5 () in
  let out = SF.run sf src in
  let producer () =
    emit src 2.0;
    pause ();
    emit src 2.0;
    pause ();
    emit src (-2.0)
  in
  let approx a b = Float.abs (a -. b) < 1e-9 in
  let consumer () =
    let a = await out in
    if not (approx a 1.0) then fail "integral first mismatch";
    let b = await out in
    if not (approx b 2.0) then fail "integral second mismatch";
    let c = await out in
    if not (approx c 1.0) then fail "integral third mismatch"
  in
  parallel [ producer; consumer ]

let () =
  execute ~instants:80 (fun _ _ ->
      test_arr_identity ();
      test_compose ();
      test_compose_associative ();
      test_fanout ();
      test_hold ();
      test_edge ();
      test_switch ();
      test_integral ())
