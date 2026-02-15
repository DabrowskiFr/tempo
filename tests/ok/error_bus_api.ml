open Tempo

let fail msg = failwith msg

let () =
  let saw_error = ref false in
  let main _ _ =
    let errors = Error_bus.signal () in
    parallel
      [ (fun () ->
          Error_bus.safe errors (fun () -> raise (Failure "boom"));
          pause ())
      ; (fun () ->
          let e = await errors in
          saw_error := true;
          match e.Error_bus.exn with
          | Failure _ -> ()
          | _ -> fail "unexpected error payload")
      ]
  in
  execute ~instants:4 main;
  if not !saw_error then fail "error not observed"
