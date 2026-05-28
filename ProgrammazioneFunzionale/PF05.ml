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


(*Sottoinsiemi con una certa somma*)

let rec sum = function
    [] -> 0
  | x::xs -> x + (sum xs)

(*Versione iterativa*)
let sum' l =
  let rec aux tot = function (* aux : int -> int list -> int *)
      [] -> tot
    | x::xs -> aux (tot+x) xs
  in aux 0 l
;;

let search_subset set tot =
  let rec search_aux solution others = (* search_aux : int list -> int list -> int list *)
    let s = sum solution
    in if s=tot then solution
       else if s>tot then raise Not_found
       else match others with  (*we have s<tot*)
              [] -> raise Not_found
            | x::xs ->
               try search_aux (x::solution) xs 
               with Not_found -> search_aux (solution) xs          
  in search_aux [] set 

(*Versione alternativa*)
let search_subset' set tot =
  let rec search_aux solution others rest = (* search_aux : int list -> int list -> int -> int list *)
    if rest=0 then solution
    else if rest<0 then raise Not_found
    else match others with  (*we have rest>0*)
           [] -> raise Not_found
         | x::xs ->
            try search_aux (x::solution) xs (rest-x)
            with Not_found -> search_aux (solution) xs rest 
  in search_aux [] set tot

(*Seconda versione alternativa*)
let search_subset'' set tot = 
  let rec search_aux solution rest = function (* search_aux : int list -> int -> int list -> int list *)
      [] -> if rest>0 then raise Not_found
            else solution (*rest < 0 is impossible*)
    | x::xs -> if x>rest then search_aux solution rest xs
               else if x=rest then solution
               else try search_aux (x::solution) (rest-x) xs
                    with Not_found -> search_aux solution rest xs
  in search_aux [] tot set
;;

(* mapcons : 'a -> 'a list list -> 'a list list *)
let rec mapcons a = function 
    [] -> []
  | l::ls -> (a::l) :: (mapcons a ls)
;;

(* search_all : int -> int list -> int list list *)
let rec search_all tot = function
    [] -> if tot>0 then []
          else [[]] (*tot < 0 is impossible*)
  | x::xs -> if x>tot then search_all tot xs
             else (mapcons x (search_all (tot-x) xs)) @ (search_all tot xs)  
;;


(*Problema delle n regine*)
                   
let scacco (i,j) (m,n) = i=m || (i+j = m+n) || (i-j=m-n)
;;

let rec combine l1 l2 =
  match (l1,l2) with
    ([],[]) -> []
  | (x::xs,y::ys) -> (x,y) :: (combine xs ys)
  | _ -> failwith "The lists don't have the same length"

(*Versione iterativa*)
let combine' l1 l2 =
  let rec aux acc l1 l2 = 
    match (l1,l2) with
      ([],[]) -> acc
    | (x::xs,y::ys) -> aux ((x,y)::acc) xs ys
    | _ -> failwith "The lists don't have the same length"
  in List.rev(aux [] l1 l2)

(*Versione iterativa alternativa, con una sola lista come argomento, meno flessibile*)
let combine'' l =
  let rec aux acc n = function
      [] -> acc
    | x::xs -> aux ((x,n)::acc) (n+1) xs
  in List.rev(aux [] 1 l)
;;

(* upto : int -> int -> int list *)
let upto m n =
  let rec aux acc m' n' = (* aux : int list -> int -> int -> int list *)
    if m'>n' then acc
    else aux (n'::acc) m' (n'-1)
  in aux [] m n
;;

(*La funzione calcola se si può aggiungere una regina alla riga m nella scacchiera board*)
let safe board m =
  let n = List.length board
  in let rec aux = function
         [] -> true
       | (i,j)::xs -> not(scacco (i,j) (m,n+1)) && (aux xs)
     in aux (combine board (upto 1 n))
;;

(*La funzione cerca una soluzione (se esiste) al problema delle dim regine aggiungendo una regina a una nuova colonna della scacchiera board*)
let rec search r board dim = 
  let c = (List.length board)+1
  in if c>dim then board
     else if r>dim then raise Not_found
     else if safe board r then
       try search 1 (board@[r]) dim
       with Not_found -> search (r+1) board dim
     else search (r+1) board dim
;;

(*La funzione trova una soluzione (se esiste) al problema delle dim regine*)
let queens dim =
  let rec search r board =
    let c = List.length board + 1
    in if c>dim then board
       else if r>dim then raise Not_found
       else if safe board r then
         try search 1 (board@[r]) 
         with Not_found -> search (r+1) board 
       else search (r+1) board
  in search 1 [] 
;;

(*La funzione trova tutte le soluzioni al problema delle dim regine*)
let queens_all dim =
  let rec search_all acc r board =
    let c = List.length board + 1
    in if c>dim then board::acc
       else if r>dim then raise Not_found
       else if safe board r then
         try (search_all acc 1 (board@[r]))@acc
         with Not_found -> (search_all acc (r+1) board)@acc
       else (search_all acc (r+1) board)@acc
  in search_all [] 1 [] 
;;
