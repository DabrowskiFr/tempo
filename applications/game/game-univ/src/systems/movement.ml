open Types

let apply_input ~bounds_w ~bounds_h (p : professor) (input : input_state) =
  let dx = (if input.right then 1.0 else 0.0) -. (if input.left then 1.0 else 0.0) in
  let dy = (if input.down then 1.0 else 0.0) -. (if input.up then 1.0 else 0.0) in
  let next_x = p.pos.x +. (dx *. p.speed) in
  let next_y = p.pos.y +. (dy *. p.speed) in
  p.pos.x <- Time_helpers.clamp next_x ~min:p.radius ~max:(bounds_w -. p.radius);
  p.pos.y <- Time_helpers.clamp next_y ~min:p.radius ~max:(bounds_h -. p.radius)
