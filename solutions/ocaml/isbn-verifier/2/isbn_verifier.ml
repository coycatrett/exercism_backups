let is_valid isbn = try
  let (j, sum) = String.fold_left (fun (j, sum) c -> 
      match c with
      | '-' -> (j, sum)
      | 'X' -> if j <> 9 then raise Exit else (j + 1, sum + 10)
      | '0'..'9' -> (j + 1, sum + (int_of_char c - int_of_char '0') * (10 - j))
      | _ -> raise Exit
    ) (0,0) isbn in
    j = 10 && sum mod 11 = 0
  with Exit -> false


  (* let rec aux i j sum = 
    if i = String.length isbn then
      j = 10 && sum mod 11 = 0
    else
      let c = isbn.[i] in
        match c with
        | '-' -> aux (i + 1) j sum
        | 'X' -> if j <> 9 then false else aux (i + 1) (j + 1) (sum + 10 * (10 - j))
        | '0' .. '9' -> aux (i + 1) (j + 1) (sum + (int_of_char c - int_of_char '0') * (10 - j))
        | _ -> false
    in
    aux 0 0 0 *)