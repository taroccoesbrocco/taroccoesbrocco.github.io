exception NotFound
;;

let rec find p = function
    [] -> raise NotFound
  | x::xs -> if p x then x
             else find p xs
;;

let find_applicata l = find (function x -> x*x < 30) l
;;

let rec takewhile p = function
    [] -> []
  | x::xs -> if p x then x::(takewhile p xs)
            else []
;;

(* La versione seguente non funziona correttamente. Perché?*)
let rec takewhile' p = function
    [] -> []
  | x::xs -> if p x then x::(takewhile p xs)
            else takewhile p xs
;;

let rec dropwhile p = function
    [] -> []
  | x::xs -> if p x then dropwhile p xs
             else x::xs
;;

let partition p l =
  let rec aux acc1 acc2 = function
      [] -> (acc1,acc2)
    | x::xs -> if p x then aux (x::acc1) acc2 xs
               else aux acc1 (x::acc2) xs
  in aux [] [] l
;;

(*Versione alternativa*)
let partition' p l = (List.filter p l, List.filter (function x -> not (p x)) l)
;;

let pairwith y l = List.map (function x -> (y,x)) l
;;

let verifica_riga n l = List.for_all ((>) n) l
;;

let verifica_matrice n ls = List.exists (List.for_all ((>) n)) ls
;;

(*Versione alternativa*)
let verifica_matrice' n ls = List.exists (verifica_riga n) ls
;;

let mem l x = List.exists ((=) x) l
;;

let non p x = not (p x)
;;
  
let setdiff l1 l2 = List.filter (non (mem l2))  l1
;;

(*Versione alternativa*)
let rec setdiff' l = function
    [] -> l
  | x::xs -> setdiff' (List.filter (function y -> y<>x) l) xs 
;;

(*Versione alternativa*)
let rec setdiff'' l = function
    [] -> l
  | x::xs -> setdiff'' (List.filter ((<>) x) l) xs 
;;

let subset l1 l2 = List.for_all (mem l2) l1
;;

let duplica l = List.map (function x -> x*2) l
;;

let mapcons ls x = List.map (function (a,b) -> (a,x::b)) ls
;;

let rec tutte_liste_con n x y = match n with
    0 -> [[]]
  | n -> let precedente = tutte_liste_con (n-1) x y
         in (List.map (List.cons x) precedente) @ (List.map (List.cons y) precedente)
;;

(*La versione seguente non funziona correttamente. Perché?*)
let rec tutte_liste_con n x y = match n with
    0 -> []
  | n -> let precedente = tutte_liste_con (n-1) x y
         in (List.map (List.cons x) precedente) @ (List.map (List.cons y) precedente)
;;

let rec interleave x = function
    [] -> [[x]]
  | y::ys -> (x::y::ys) :: List.map (List.cons y) (interleave x ys) 
;;

let rec permut = function
    [] -> [[]]
  | x::xs -> List.flatten (List.map (interleave x)  (permut xs))
;;

(*La versione seguente non funziona. Perché?*)
let rec permut = function
    [] -> [[]]
  | x::xs -> List.(List.map (interleave x)  (permut xs)
;;

let in_riga matrix r v = List.exists (function ((x,y),z) -> x=r && z=v)  (snd matrix)
;;

let trova_colonna matrix r v = snd (fst (List.find (function ((x,y),z) -> x=r && z=v) (snd matrix)))
;;

let in_tutte matrix v = List.map (List.find (function ((x,y),z) -> z=r)) (snd matrix)


let find x l =
  let rec aux acc1 acc2 = function
      [] -> if acc2=[] then raise NotFound
            else (acc1,acc2)
    | y::ys -> if x=y then (acc1,acc2@(y::ys))
               else aux (acc1@[y]) acc2 ys
  in aux [] [] l
;;

let spezza x l =
  let (a,b) = find x (List.tl (snd (find x l)))
  in (a, List.tl b)
;;

let rec prendi p = function
    [] -> raise NotFound
  | x::xs -> if p x then (x, xs)
             else prendi p xs
;;
