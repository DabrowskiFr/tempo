open Tempo

let scenario () =
  let payload = new_signal () in
  let consumer () =
    Format.printf "[consumer] waiting with await_immediate@.%!";
    let value = await_immediate payload in
    Format.printf "[consumer] received %d@." value
  and producer () =
    Format.printf "[producer] emit value@.%!";
    emit payload 42
  in
  parallel [ consumer; producer ]

let () = execute (fun _ _ -> scenario ())
