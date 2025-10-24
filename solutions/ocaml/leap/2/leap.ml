(* divisible by 4 and (if divisible by 100 then divisible by 400) *)
let leap_year year = 
    Int.logand year 3 = 0 && 
    ((Int.shift_right year 2) mod 100 = 0 || year mod 100 <> 0)
