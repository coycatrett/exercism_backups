let leap_year year = 
    if Int.logand year 3 = 0 then (* divisible by 4 *)
        if year mod 100 = 0 then  (* divisible by 100 *)
            if (Int.shift_right year 2) mod 100 = 0 then
                true  (* divisible by 400 *)
            else false  (* divisible by 100 but not 400 *)
        else true  (* divisible by 4 but not 100 *)
    else false  (* not divisible by 4 *)