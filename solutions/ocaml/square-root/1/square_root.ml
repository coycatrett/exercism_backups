let square_root num = 
  let rec aux l h = 
    let m = l + (h - l) / 2 in
    if m * m = num then 
      m
    else if m * m < num then
      aux (m + 1) h
    else
      aux l (m - 1)
  in
  aux 0 (num / 2)
  