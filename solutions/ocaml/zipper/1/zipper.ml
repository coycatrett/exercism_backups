open Sexplib0.Sexp_conv

type 'a t = { zipper: 'a option array; idx: int ref }
    [@@deriving sexp]

let parent_idx idx = 
    if Int.rem idx 2 = 0 then 
        Int.div (idx - 1) 2
    else
        Int.div idx 2

let left_child_idx idx = 2 * idx + 1
let right_child_idx idx = 2 * (idx + 1)

let equal veq z1 z2 = Array.equal (Option.equal veq) (z1.zipper) (z2.zipper) && z1.idx = z2.idx

let of_tree (tree : 'a Tree.t) : 'a t =
    let rec size (tree : 'a Tree.t) = 
        match tree.left, tree.right with
        | None, None -> 1
        | Some l, None -> 1 + size l
        | None, Some r -> 1 + size r
        | Some l, Some r -> 1 + size l + size r
    in
    let n = size tree in
    let zipper = Array.make (Int.shift_left 1 n) (None) in
    let rec aux (tree : 'a Tree.t) idx = 
        if idx < Array.length zipper then begin
            zipper.(idx) <- Some tree.value;
      
            (match tree.left with
             | None -> ()
             | Some l -> aux l (left_child_idx idx));
      
            (match tree.right with
             | None -> ()
             | Some r -> aux r (right_child_idx idx))
          end
    in
    aux tree 0;
    {zipper = zipper; idx = ref 0}


let to_tree (z : 'a t) : 'a Tree.t =
    let rec aux idx : 'a Tree.t option = 
        if idx >= Array.length z.zipper then None else
        match (z.zipper).(idx) with
        | None -> None
        | Some v -> Some {
                value = v; 
                left = aux (2 * idx + 1); 
                right = aux (2 * (idx + 1));
            }
    in 
    match aux 0 with 
    | None -> failwith "Impossible"
    | Some t -> t


let value (z : 'a t) : 'a = 
    match (z.zipper).(!(z.idx)) with 
    | None -> failwith "impossible"
    | Some v -> v


let left (z : 'a t) = 
    let left_idx = left_child_idx !(z.idx) in
    if left_idx >= Array.length z.zipper then None else 
    match (z.zipper).(left_idx) with
    | None -> None
    | Some _ -> z.idx := left_idx; Some z

let right (z : 'a t) = 
    let right_idx = right_child_idx !(z.idx) in
    if right_idx >= Array.length z.zipper then None else 
    match (z.zipper).(right_idx) with
    | None -> None
    | Some _ -> z.idx := right_idx; Some z

let up (z : 'a t) = 
    let parent_idx = parent_idx !(z.idx) in
    match (z.zipper).(parent_idx) with
    | None -> None
    | Some _ -> z.idx := parent_idx; Some z

let set_value (v : 'a) (z : 'a t) = (z.zipper).(!(z.idx)) <- Some v; z

let set_left (t_opt : 'a Tree.t option) (z : 'a t) = 
    let left_idx = left_child_idx !(z.idx) in
    if left_idx >= Array.length z.zipper then () else
    (match t_opt with
    | None -> (z.zipper).(left_idx) <- None
    | Some t -> (z.zipper).(left_idx) <- Some t.value);
    z

let set_right (t_opt : 'a Tree.t option) (z : 'a t) = 
    let right_idx = right_child_idx !(z.idx) in
    if right_idx >= Array.length z.zipper then () else
    (match t_opt with
    | None -> (z.zipper).(right_idx) <- None
    | Some t -> (z.zipper).(right_idx) <- Some t.value);
    z
