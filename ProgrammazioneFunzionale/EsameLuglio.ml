let rec exists_rec p = function
    [] -> false
  | x::xs -> if p x then true else exists_rec p xs

(*Versione alternativa*)
let rec exists_rec p = function
    [] -> false
  | x::xs -> p x || exists_rec p xs

let exists_fold p l = List.fold_right (function x -> (||) (p x)) l false 

(*Versione alternativa*)
let exists_fold p l = List.fold_left (function x -> function y -> (p y) || x) false l

let exists_lib p l = try [List.find p l] <> [] 
                  with _ -> false

(*Versione alternativa*)
let exists_lib p l = List.filter p l <> []
;;


type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let t = Tr(5, Tr(2, Tr(1,Empty,Empty), Tr(4,Tr(3,Empty,Empty),Empty)), Tr(7,Empty,Tr(9,Empty,Empty)))

let t' = Tr(7,Tr(5,Empty,Empty),Empty)

let rec leftmost = function
    Empty -> raise Not_found
  | Tr(x,Empty,_) -> x
  | Tr(_,t,_) -> leftmost t

let rec rightmost = function
    Empty -> raise Not_found
  | Tr(x,_,Empty) -> x
  | Tr(_,_,t) -> rightmost t

let rec is_bst_with_min_max minval maxval = function
    Empty -> true
  | Tr(x,t1,t2) -> minval < x && x < maxval && is_bst_with_min_max minval x t1 && is_bst_with_min_max x maxval t2

let is_bst_int t = t=Empty || is_bst_with_min_max (leftmost t - 1) (rightmost t + 1) t
;;
