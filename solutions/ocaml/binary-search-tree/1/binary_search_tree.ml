open Base

type bst = Empty | Node of bst * int * bst

let empty = Empty

let value node = match node with Empty -> Error "empty node has not value" | Node (_,v,_) -> Ok v

let left t = match t with Empty -> Error "empty tree has no left subtree" | Node (l,_,_) -> Ok l

let right t = match t with Empty -> Error "empty tree has no right subtree" | Node (_,_,r) -> Ok r

let rec insert v t = 
  match t with
  | Empty -> Node (Empty, v, Empty)
  | Node (l,u,r) -> 
    if v <= u then Node (insert v l, u, r)
    else Node (l, u, insert v r)

let to_list t = (* in order traversal *)
  let rec aux acc = function
    | Empty -> acc
    | Node (l,v,r) -> aux (v :: aux acc r) l
  in aux [] t

