let egg_count = 
  let rec shift_count count = function
    | 0 -> count
    | n -> shift_count (count + n land 1) (n lsr 1)
  in shift_count 0