type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let add_tree left right lst = (Tr('x',left,right)) :: lst

(* Se left è un albero binario, poiché il tipo di add_tree è char tree -> char tree -> char tree list -> char tree list, allora il tipo di add_tree left è char tree -> char tree list -> char tree list *)
;;

let add_left_to_rights left rights lst = List.fold_right (add_tree left) rights lst
;;

let add_lefts_to_rights lefts rights lst = List.fold_right (fun l -> add_left_to_rights l rights) lefts lst
;;

let rec bal_trees n =
  if n = 0 then [Empty]
  else if (n mod 2 = 1) then let ts = bal_trees (n/2)
                             in add_lefts_to_rights ts ts []
  else  add_lefts_to_rights (bal_trees (n/2-1)) (bal_trees (n/2)) (add_lefts_to_rights (bal_trees (n/2)) (bal_trees (n/2-1)) [])
;;
