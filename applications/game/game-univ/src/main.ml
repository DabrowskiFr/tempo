let () =
  let world = Game.initial_world () in
  Raylib_platform.run ~spec:Raylib_adapter.spec (fun input output ->
      let bus = Bus.create ~input ~output in
      Game.process bus world)
