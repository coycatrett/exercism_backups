let sounds = [(3, "Pling"); (5, "Plang"); (7, "Plong")]
let raindrop n =
  let to_sound (factor, sound) = if n mod factor = 0 then sound else "" in
  match String.concat "" (List.map to_sound sounds) with
  | "" -> Int.to_string n
  | sound -> sound

