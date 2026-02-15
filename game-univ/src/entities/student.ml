open Types

let process (bus : Bus.t) (student : student) =
  let rec loop frame =
    let _ = Tempo.await bus.input in
    let cheating_now = ((frame + (student.id * 17)) mod 240) < 110 in
    if cheating_now <> student.cheating then
      Tempo.emit bus.evt
        [ if cheating_now then Cheating_start student.id else Cheating_stop student.id ];
    student.cheating <- cheating_now;
    loop (frame + 1)
  in
  loop 0
