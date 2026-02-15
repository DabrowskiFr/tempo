open Tempo

let fail msg = failwith msg

type msg =
  | Burst
  | Trigger
  | Inc

let program : (int, msg) App.program =
  {
    init = 0;
    update =
      (fun model m ->
        match m with
        | Inc -> (model + 1, App.none)
        | Burst -> (model, App.batch [ App.emit Inc; App.emit Inc ])
        | Trigger -> (model, App.after_n 2 Inc));
  }

let () =
  let script = ref [ None; Some Burst; Some Trigger; None; None; None ] in
  let input () =
    match !script with
    | [] -> None
    | x :: xs ->
        script := xs;
        x
  in
  let outputs = ref [] in
  App.run_with_view ~instants:10 ~input ~view:Fun.id
    ~output:(fun v -> outputs := v :: !outputs)
    program;
  let got = List.rev !outputs in
  let expected = [ 0; 2; 3 ] in
  if got <> expected then
    fail
      (Format.asprintf "app view trace mismatch: expected [%s], got [%s]"
         (String.concat "; " (List.map string_of_int expected))
         (String.concat "; " (List.map string_of_int got)))
