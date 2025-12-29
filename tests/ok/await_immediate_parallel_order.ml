open Tempo

let run_case ~producer_first =
  execute_trace ~instants:2 ~inputs:[ None; None ] (fun _input output ->
      let s = new_signal () in
      let waiter () =
        let v = await_immediate s in
        emit output v
      in
      let producer () = emit s 42 in
      if producer_first then parallel [ producer; waiter ] else parallel [ waiter; producer ])

let () =
  let a = run_case ~producer_first:true in
  let b = run_case ~producer_first:false in
  Printf.printf "producer-first: %s\n"
    (String.concat "," (List.map string_of_int a));
  Printf.printf "waiter-first: %s\n"
    (String.concat "," (List.map string_of_int b));
  Printf.printf "same-result: %b\n" (a = b)
