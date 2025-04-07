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

exception NotFound ;;

let search_subset set tot =
  let rec search_aux solution others tot' =
    let s = sum' solution
    in if s=tot' then solution
       else if s>tot' then raise NotFound 
       else match others with  (*we have s<tot*)
              [] -> raise NotFound
            | x::xs ->
               try search_aux (x::solution) xs tot'
               with NotFound -> search_aux (solution) xs tot'         
  in search_aux [] set tot
;;

(*Versione alternativa*)
let search_subset' set tot =
  let rec search_aux solution others rest =
    if rest=0 then solution
    else if rest<0 then raise NotFound
    else match others with  (*we have rest>0*)
           [] -> raise NotFound
         | x::xs ->
            try search_aux (x::solution) xs (rest-x)
            with NotFound -> search_aux (solution) xs rest 
  in search_aux [] set tot
;;

(*Seconda versione alternativa*)
let search_subset'' set tot = 
  let rec search_aux solution rest = function
      [] -> if rest>0 then raise NotFound
            else solution (*rest < 0 is impossible*)
    | x::xs -> if x>rest then search_aux solution rest xs
               else if x=rest then solution
               else try search_aux (x::solution) (rest-x) xs
                    with NotFound -> search_aux solution rest xs
  in search_aux [] tot set
;;

let rec mapcons a = function
    [] -> []
  | l::ls -> (a::l) :: (mapcons a ls)
;;

let rec search_all tot = function
    [] -> if tot>0 then []
          else [[]] (*tot < 0 is impossible*)
  | x::xs -> if x>tot then search_all tot xs
             else (mapcons x (search_all (tot-x) xs)) @ (search_all tot xs)  
;;
                   
