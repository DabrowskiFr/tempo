open Tempo

module Frp = struct
  let map f s =
    let out = new_signal () in
    let rec loop () =
      let v = await s in
      emit out (f v);
      loop ()
    in
    ignore (Low_level.fork loop);
    out

  let filter p s =
    let out = new_signal () in
    let rec loop () =
      let v = await s in
      if p v then emit out v;
      loop ()
    in
    ignore (Low_level.fork loop);
    out

  let fold ~initial f s =
    let st = new_state initial in
    let rec loop () =
      let v = await s in
      modify_state st (fun acc -> f acc v);
      loop ()
    in
    ignore (Low_level.fork loop);
    st

  let hold ~initial s =
    let st = new_state initial in
    let rec loop () =
      let v = await s in
      set_state st v;
      loop ()
    in
    ignore (Low_level.fork loop);
    st

  let sample st = get_state st

  let once s =
    let out = new_signal () in
    let proc () =
      let v = await s in
      emit out v;
      let rec done_forever () =
        pause ();
        done_forever ()
      in
      done_forever ()
    in
    ignore (Low_level.fork proc);
    out

  let edge b =
    let out = new_signal () in
    let rec loop prev =
      let curr = await b in
      if curr && not prev then emit out ();
      loop curr
    in
    ignore (Low_level.fork (fun () -> loop false));
    out

  let throttle_n n s =
    if n < 0 then invalid_arg "Tempo_frp.Frp.throttle_n: n must be >= 0";
    let out = new_signal () in
    let rec sleep k =
      if k <= 0 then ()
      else (
        pause ();
        sleep (k - 1))
    in
    let rec loop () =
      let v = await s in
      emit out v;
      sleep n;
      loop ()
    in
    ignore (Low_level.fork loop);
    out

  let debounce_n n s =
    if n < 0 then invalid_arg "Tempo_frp.Frp.debounce_n: n must be >= 0";
    let out = new_signal () in
    let restart = new_signal () in
    let latest = new_state None in
    let collector () =
      let rec loop () =
        let v = await s in
        set_state latest (Some v);
        emit restart ();
        loop ()
      in
      loop ()
    in
    let timers () =
      let rec loop () =
        let _ = await restart in
        ignore
          (Low_level.fork (fun () ->
               watch restart (fun () ->
                   Game.after_n n (fun () ->
                       match get_state latest with Some v -> emit out v | None -> ()))));
        loop ()
      in
      loop ()
    in
    ignore (Low_level.fork (fun () -> parallel [ collector; timers ]));
    out

  let switch_once trigger make =
    let v = await trigger in
    make v ()

  let switch_latest trigger make =
    let rec run current =
      let switched = new_signal () in
      parallel
        [
          (fun () -> watch switched (fun () -> make current ()));
          (fun () ->
            let next = await trigger in
            emit switched ();
            run next);
        ]
    in
    let first = await trigger in
    run first
end

module SF = struct
  module Event = struct
    type 'a t =
      | NoEvent
      | Event of 'a

    let map f = function NoEvent -> NoEvent | Event x -> Event (f x)
    let is_event = function NoEvent -> false | Event _ -> true
  end

  type ('a, 'b) t = { run : 'a signal -> 'b signal }

  let run sf s = sf.run s
  let arr f = { run = (fun s -> Frp.map f s) }
  let compose a b = { run = (fun s -> b.run (a.run s)) }
  let ( >>> ) a b = compose a b

  let fanout a b =
    {
      run =
        (fun s ->
          let sa = a.run s in
          let sb = b.run s in
          let out = new_signal () in
          let rec loop () =
            let _ = await s in
            let va = await_immediate sa in
            let vb = await_immediate sb in
            emit out (va, vb);
            loop ()
          in
          ignore (Low_level.fork loop);
          out);
    }

  let ( &&& ) a b = fanout a b

  let hold ~initial sf =
    {
      run =
        (fun s ->
          let evs = sf.run s in
          let out = new_signal () in
          let current = ref initial in
          let rec loop () =
            (match await evs with Event.Event v -> current := v | Event.NoEvent -> ());
            emit out !current;
            loop ()
          in
          ignore (Low_level.fork loop);
          out);
    }

  let edge sf_bool =
    {
      run =
        (fun s ->
          let bs = sf_bool.run s in
          let out = new_signal () in
          let prev = ref false in
          let rec loop () =
            let b = await bs in
            let ev = if (not !prev) && b then Event.Event () else Event.NoEvent in
            prev := b;
            emit out ev;
            loop ()
          in
          ignore (Low_level.fork loop);
          out);
    }

  let switch sf0 k =
    {
      run =
        (fun s ->
          let out = new_signal () in
          let rec run_pre ys =
            let b, evt = await ys in
            emit out b;
            match evt with Event.NoEvent -> run_pre ys | Event.Event c -> run_post ((k c).run s)
          and run_post ys =
            let b = await ys in
            emit out b;
            run_post ys
          in
          ignore (Low_level.fork (fun () -> run_pre (sf0.run s)));
          out);
    }

  let integral ?(initial = 0.0) ~dt () =
    if dt < 0.0 then invalid_arg "Tempo_frp.SF.integral: dt must be >= 0";
    {
      run =
        (fun s ->
          let out = new_signal () in
          let acc = ref initial in
          let rec loop () =
            let v = await s in
            acc := !acc +. (v *. dt);
            emit out !acc;
            loop ()
          in
          ignore (Low_level.fork loop);
          out);
    }
end
