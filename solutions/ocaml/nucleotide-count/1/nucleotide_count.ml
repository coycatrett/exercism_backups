open Base
let empty = Map.empty (module Char)

let is_nucl = function 'A' | 'C' | 'G' | 'T' -> true | _ -> false

let count_nucleotide s c = if not (is_nucl c) then Error c
  else String.fold_result s ~init:0 ~f:(
    fun count ch -> 
      if Char.equal ch c then Ok (count + 1)
      else if is_nucl ch then Ok count
      else Error ch
    )
  let count_nucleotides s = String.fold_result s ~init:empty ~f:(
    fun m c -> 
      if is_nucl c then Ok (Map.update m c ~f:(Option.value_map ~f:((+) 1) ~default:1))
      else Error c
    )