let find a x =
    let rec aux l h =
        if l > h then 
            Result.Error("value not in array")
        else 
            let m = l + Int.div (h - l) 2 in
            if a.(m) = x then 
                Result.Ok(m)
            else if a.(m) < x then
                aux (m + 1) h
            else
                aux l (h - 1)
    in 
    aux 0 (Array.length a - 1)