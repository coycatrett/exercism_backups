module Circle = struct
  type t =
    | Inner
    | Middle
    | Outer
    | Outside

  let classify x =
    if 0. <= x && x <= 1. then Inner
    else if  1. < x && x <=  25. then Middle
    else if 25. < x && x <= 100. then Outer
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