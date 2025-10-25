let is_pangram s = 
  let chars = Array.make 26 false in
  let update c = (
    let code = Char.code (Char.lowercase_ascii c) in 
      if 97 <= code && code <= 122 then
        chars.(code - Char.code 'a') <- true;
    ) in String.iter update s; Array.for_all Fun.id chars;