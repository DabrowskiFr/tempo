open Tempo

let fail msg = failwith msg

let () =
  let tags = Tick_tags.create () in
  Tick_tags.mark tags "boot";
  Tick_tags.mark tags "scene:menu";
  if Tick_tags.all tags <> [ "boot"; "scene:menu" ] then fail "tag order mismatch";
  let main _ _ =
    let st = State.create 7 in
    let snap = Runtime_snapshot.capture ~tags ~frame:12 () in
    State.set st 99;
    Runtime_snapshot.restore snap;
    if State.get st <> 7 then fail "snapshot restore failed";
    if Runtime_snapshot.frame snap <> 12 then fail "snapshot frame mismatch";
    if Runtime_snapshot.tags snap <> [ "boot"; "scene:menu" ] then fail "snapshot tags mismatch"
  in
  execute ~instants:2 main;
  Tick_tags.clear tags;
  if Tick_tags.all tags <> [] then fail "tick tags clear failed";
  require_api_level 1
