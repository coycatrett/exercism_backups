let raindrop n =
  let res = ref "" in
  if n mod 3 = 0 then res := !res ^ "Pling";
  if n mod 5 = 0 then res := !res ^ "Plang";
  if n mod 7 = 0 then res := !res ^ "Plong";
  if !res = "" then Int.to_string n else !res
