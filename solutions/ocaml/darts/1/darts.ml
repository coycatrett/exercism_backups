module Circle = struct
  type t =
    | Inner
    | Middle
    | Outer
    | Outside

  let classify x =
    if x >= 0. && x <= 1. then Inner
    else if x > 1. && x <= 25. then Middle
    else if x > 25. && x <= 100. then Outer
    else Outside
end

let score (x: float) (y: float): int =
  let r_squared = (x *. x) +. (y *. y) in Circle.(
    match classify r_squared with
    | Inner   -> 10
    | Middle  -> 5
    | Outer   -> 1
    | Outside -> 0
  )