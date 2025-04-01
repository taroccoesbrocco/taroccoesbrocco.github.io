let rec mem x = function
    [] -> false
  | y::ys -> if x=y then true
             else mem x ys
;;

let rec union l = function
    [] -> l
  | x::xs -> if mem x l then union l xs
             else x::(union l xs)
;;

(*Versione iterativa*)
let union' l1 l2 =
  let rec aux acc l = function
      [] -> acc@l
    | x::xs -> if mem x l then aux acc l xs
               else aux (x::acc) l xs
  in aux [] l1 l2
;;

(*Versione iterativa alternativa*)
let rec union'' l = function
    [] -> l
  | x::xs -> if mem x l then union'' l xs
             else union'' (x::l) xs
;;

let rec intersect l = function
    [] -> []
  | x::xs -> if mem x l then x::(intersect l xs)
             else intersect l xs
;;

(*Versione iterativa*)
let intersect' l1 l2 =
  let rec aux acc l = function
      [] -> acc
    | x::xs -> if mem x l then aux (x::acc) l xs
               else aux acc l xs
  in aux [] l1 l2
;;

(*La versione seguente non è corretta. Perché?*)
let rec intersect'' l = function
    [] -> []
  | x::xs -> if mem x l then intersect'' (x::l) xs
             else intersect'' l xs
;;


let rec setdiff l l' = match l with
    [] -> []
  | x::xs -> if mem x l' then setdiff xs l'
             else x::(setdiff xs l')
;;

(*Versione iterativa*)
let setdiff' l1 l2 =
  let rec aux acc l = function
      [] -> acc
    | x::xs -> if mem x l then aux acc l xs
               else aux (x::acc) l xs
  in aux [] l2 l1
;;

(*La versione seguente di setdiff non è corretta. Perché? Cosa calcola?*)
let setdiff'' l1 l2 =
  let rec aux acc l = function
      [] -> acc
    | x::xs -> if mem x l then aux acc l xs
               else aux (x::acc) l xs
  in aux [] l1 l2
;;

let rec sum = function
    [] -> 0
  | x::xs -> x + (sum xs)
;;

let sum' l =
  let rec aux tot = function
      [] -> tot
    | x::xs -> aux (tot+x) xs
  in aux 0 l
;;

let rec solve sol set tot =
  let s = sum' sol
  in if s=tot then sol
     else if s<tot then solve (List.hd set :: sol) (List.tl set) tot
     else solve (setdiff sol (List.hd sol)) 
