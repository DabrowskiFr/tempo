let clamp v ~min ~max =
  if v < min then min else if v > max then max else v

let dist_sq (ax, ay) (bx, by) =
  let dx = ax -. bx in
  let dy = ay -. by in
  (dx *. dx) +. (dy *. dy)
