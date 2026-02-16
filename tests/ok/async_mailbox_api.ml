open Tempo
open Tempo_async

let fail msg = failwith msg

let test_overflow_drop_oldest () =
  let mb = create ~capacity:2 ~overflow:`Drop_oldest () in
  ignore (push_external mb 1);
  ignore (push_external mb 2);
  let r = push_external mb 3 in
  if r <> `Dropped then fail "drop_oldest should report dropped";
  let got = drain_tick mb in
  if got <> [ 2; 3 ] then fail "drop_oldest FIFO mismatch";
  let s = stats mb in
  if s.dropped_oldest <> 1 then fail "drop_oldest counter mismatch"

let test_overflow_reject () =
  let mb = create ~capacity:1 ~overflow:`Reject () in
  ignore (push_external mb "a");
  let r = push_external mb "b" in
  if r <> `Dropped then fail "reject should report dropped";
  let got = drain_tick mb in
  if got <> [ "a" ] then fail "reject should keep oldest payload";
  let s = stats mb in
  if s.rejected <> 1 then fail "reject counter mismatch"

let test_pump_to_bus () =
  let mb = create ~capacity:4 ~overflow:`Drop_newest () in
  ignore (push_external mb "one");
  ignore (push_external mb "two");
  let outs =
    execute_trace ~instants:4 ~inputs:[ None ] (fun _input output ->
        let bus = Event_bus.channel () in
        parallel
          [ (fun () ->
              let n = pump_to_bus mb bus in
              if n <> 2 then fail "pump_to_bus count mismatch")
          ; (fun () ->
              let batch = Event_bus.await_batch bus in
              emit output batch)
          ])
  in
  match outs with
  | [ batch ] ->
      if batch <> [ "one"; "two" ] then fail "pump_to_bus payload mismatch"
  | _ -> fail "unexpected trace shape"

let () =
  test_overflow_drop_oldest ();
  test_overflow_reject ();
  test_pump_to_bus ()
