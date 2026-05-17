open Tempo

let body_entered = ref false
let payload_value = ref None
let worker_done = ref false

let worker guard payload () =
  when_ guard (fun () ->
      body_entered := true;
      payload_value := Some (await_immediate payload));
  worker_done := true

let driver guard payload () =
  emit guard ();
  emit payload 42

let () =
  execute (fun _ _ ->
      let guard = new_signal () in
      let payload = new_signal () in
      parallel [ driver guard payload; worker guard payload ]);
  if not !body_entered then failwith "guarded body did not run";
  if not !worker_done then failwith "worker continuation did not run";
  if !payload_value <> Some 42 then failwith "await_immediate did not get payload";
  Format.printf "body_entered=%b@.%!" !body_entered;
  Format.printf "payload=%d@.%!"
    (match !payload_value with Some v -> v | None -> -1);
  Format.printf "worker_done=%b@.%!" !worker_done
