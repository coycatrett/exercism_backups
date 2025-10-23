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