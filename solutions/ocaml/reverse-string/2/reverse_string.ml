let reverse_string s = 
  let n = String.length s in String.concat "" (List.init n (fun i -> String.make 1 s.[n - 1 - i]))
