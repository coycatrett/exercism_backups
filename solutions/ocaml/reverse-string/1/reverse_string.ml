let reverse_string s = String.fold_left (fun s c -> (String.make 1 c) ^ s) "" s
