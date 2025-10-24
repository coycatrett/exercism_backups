type nucleotide = A | C | G | T

let hamming_distance l1 l2 =
  let rec aux dist = function
  | [],[] -> Ok dist
  | hd1 :: tl1, hd2 :: tl2 -> aux (dist + Bool.to_int (hd1 <> hd2)) (tl1, tl2)
  | _, _ -> Error "strands must be of equal length"
in aux 0 (l1, l2)