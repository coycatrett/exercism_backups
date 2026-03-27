let is_valid isbn = 
  let res = Base.String.fold_result isbn ~init:(0,0) ~f:(fun (length, sum) c -> 
    match c with
    | '-' -> Ok(length, sum)
    | 'X' -> 
      if length <> 9 then Error "Early X"
      else Ok(length + 1, sum + 10)
    | '0'..'9' -> 
      let c_int = int_of_char c - int_of_char '0' in
      Ok(length + 1, sum + c_int * (10 - length))
    | _ -> Error "Invalid Char"
  ) in 
  match res with 
  | Ok(length, sum) -> length = 10 && sum mod 11 = 0
  | Error _ -> false
  
  
  (* try
  let (j, sum) = String.fold_left (fun (j, sum) c -> 
      match c with
      | '-' -> (j, sum)
      | 'X' -> if j <> 9 then raise Exit else (j + 1, sum + 10)
      | '0'..'9' -> (j + 1, sum + (int_of_char c - int_of_char '0') * (10 - j))
      | _ -> raise Exit
    ) (0,0) isbn in
    j = 10 && sum mod 11 = 0
  with Exit -> false *)