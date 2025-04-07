let rec length = function
    [] -> 0
  | _::xs -> 1 + (length xs)
;;

(*Versione iterativa*)
let length' l = 
  let rec aux acc = function
      [] -> acc
    | _::xs -> aux (1+acc) xs
  in aux 0 l
;;

let rec sumof = function
    [] -> 0
  | x::xs -> x + (sumof xs)
;;

(*Versione iterativa*)
let sumof' l =
  let rec aux acc = function
      [] -> acc
    | x::xs -> aux (x+acc) xs
  in aux 0 l
;;

exception NoElements

let maxlist l =
  try let x = List.hd l
      in let rec aux m = function
             [] -> m
           | y::ys -> if y>m then aux y ys
                      else aux m ys
         in aux x (List.tl l)
  with _ -> raise NoElements
;;

(*Versione alternativa*)
let maxlist' l =
  let rec aux m = function
             [] -> m
           | y::ys -> if y>m then aux y ys
                      else aux m ys
  in try aux (List.hd l) (List.tl l)
     with _ -> raise NoElements
;;

let rec drop n = function
    [] -> []
  | l -> if n>0 then drop (n-1) (List.tl l)
         else l
;;

let rec append l1 l2 = match l1 with
    [] -> l2
  | x::xs -> x::(append xs l2)
;;

(*Versione iterativa*)
let append' l1 l2 =
  let rec aux acc = function
      [] -> acc
    | x::xs -> aux (x::acc) xs
  in aux l2 (List.rev l1)
;;

(*La versione seguente non funziona correttamente. Perché?*)
let append'' l1 l2 =
  let rec aux acc = function
      [] -> acc
    | x::xs -> aux (x::acc) xs
  in aux l2 l1
;;

(*La versione seguente non funziona correttamente. Perché?*)
let rec append''' l = function
    [] -> l
  | x::xs -> x::(append''' xs l)
;;


let rec reverse = function
    [] -> []
  | x::xs -> (reverse xs)@[x]
;;

(*Versione iterativa*)
let reverse l =
  let rec aux acc = function
      [] -> acc
    | x::xs -> aux (x::acc) xs
  in aux [] l
;;

let rec nth n = function
    [] -> raise NoElements
  | x::xs -> if n=0 then x
             else nth (n-1) xs
;;

(*Versione alternativa, più performante*)
let rec nth' n l =
  if n<0 then raise NoElements
  else match l with
         [] -> raise NoElements
       | x::xs -> if n=0 then x
             else nth' (n-1) xs
;;

let rec remove x = function
    [] -> []
  | y::ys -> if x=y then remove x ys
             else y::(remove x ys)
;;

(*Versione iterativa*)
let remove' x l =
  let rec aux acc x = function
      [] -> acc
    | y::ys -> if x=y then aux acc x ys
               else aux (y::acc) x ys
  in aux [] x (List.rev l)
;;

let rec copy n x =
  if n<=0 then []
  else x::(copy (n-1) x)
;;

(*Versione iterativa*)
let copy' n' x =
  let rec aux acc n =
    if n<=0 then acc
    else aux (x::acc) (n-1)
  in aux [] n'
;;

(*La versione seguente non è corretta. Perché?*)
let copy' n x =
  let rec aux acc =
    if n<=0 then acc
    else aux (x::acc) 
  in aux [] 
;;

let rec nondec = function
    [] | [x] -> true
    | x::y::xs -> if x<=y then nondec (y::xs)
                  else false
;;

(*La versione seguente non funcziona. Perché?*)
let rec nondec = function
    [] | [x] -> true
    | x::y::xs -> if x<=y then nondec xs
                  else false
;;

let rec pairwith y = function
    [] -> [] 
  | x::xs -> (y,x)::(pairwith y xs)
;;

(*Versione iterativa*)
let pairwith' y' l =
  let rec aux acc y = function
      [] -> acc 
    | x::xs -> aux ((y,x)::acc) y xs
  in aux [] y' (List.rev l)
;;

let rec duplica l = function
    [] -> []
  | x::xs -> x::x::(duplica xs)
;;

(*Versione iterativa*)
let duplica' l =
  let rec aux acc = function
      [] -> acc
    | x::xs -> aux (x::x::acc) xs
  in aux [] (List.rev l)
;;

let enumera l =
  let rec aux acc n = function
      [] -> acc
    | x::xs -> aux ((n,x)::acc) (n-1) xs
  in aux [] (List.length l - 1) (List.rev l)
;;

(*La versione seguente non è corretta. Perché?*)
let enumera l =
  let rec aux acc n = function
      [] -> acc
    | x::xs -> aux ((n,x)::acc) (n+1) xs
  in aux [] 0 (List.rev l)
;;

let position x l = 
    let rec aux n x' = function 
        [] -> raise NoElements
      | y::ys -> if y=x' then n
                 else aux (n+1) x' ys
    in aux 0 x l
;;

let alternate l =
  let rec aux acc n = function
      [] -> acc
    | x::xs -> if (n mod 2)=0 then aux (x::acc) (n-1) xs
               else aux acc (n-1) xs
  in aux [] (List.length l) (List.rev l)
;;

(*Versione alternativa*)
let alternate' l =
  let rec aux acc n = function
      [] -> acc
    | x::xs -> if (n mod 2)=1 then aux (acc@[x]) (n+1) xs
               else aux acc (n+1) xs
  in aux [] 0 l
;;

(*La versione seguente non è corretta. Perché?*)
let alternate'' l =
  let rec aux acc n = function
      [] -> acc
    | x::xs -> if (n mod 2)=0 then aux (acc@[x]) (n+1) xs
               else aux acc (n+1) xs
  in aux [] 0 l
;;

let minlist l =
  let rec aux m = function
          [] -> m
        | x::xs -> if x<m then aux x xs
                   else aux m xs
  in try aux (List.hd l) (List.tl l)
     with _ -> raise NoElements
;;

let min_dei_max ls =
  let rec aux acc = function
      [] -> acc
    | x::xs -> aux ((maxlist x)::acc) xs
  in minlist (aux [] ls)
;;

let split2 l =
  let n = (List.length l)/2
  in (take n l, drop n l)
;;
