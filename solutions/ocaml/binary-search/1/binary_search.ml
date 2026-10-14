let find a x =
    let n = Array.length a in
    let l, h, m = ref 0, ref (n - 1), ref 0 in
    let res = ref (Result.Error("value not in array")) in
    while !l <= !h do
        m := !l + Int.div (!h - !l) 2;

        if a.(!m) = x then (
            res := Result.Ok(!m);
            l := !h + 1) 
        else if a.(!m) < x then 
            l := !m + 1 
        else 
            h := !m - 1
    done;
    !res