let egg_count n = 
  let rec shift_count n count = 
    match n with
    | 0 -> count
    | _ -> let n_lsr = n lsr 1 in 
    shift_count n_lsr ((fun b -> if b then count else count + 1) (n_lsr lsl 1 = n))
  in shift_count n 0