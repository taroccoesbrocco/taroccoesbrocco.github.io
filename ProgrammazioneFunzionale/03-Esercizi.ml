exception BadTime

let somma_ore (h1,m1) (h2,m2) =
  if (0<=h1 && h1<24) && (0<=h2 && h2<24) && (0<=m1 && m1<60) && (0<=m2 && m2<60) then
    let m = m1 + m2
    in ((h1 + h2 + m/60) mod 24, m mod 60)
  else raise BadTime

(*Versione alternativa*)
let somma_ore' (h1,m1) (h2,m2) =
  if (0<=h1 && h1<24) && (0<=h2 && h2<24) && (0<=m1 && m1<60) && (0<=m2 && m2<60) then
    let m = m1 + m2
    in ((h1 + h2 + m/60) mod 24, m mod 60)
  else failwith "One of the dates is not correct."
;;

exception NoInput 

let read_max () =
  try let m = read_int()
      in let rec aux m' =
           try let n = read_int()
               in aux (max n m')
           with _ -> m'
         in aux m 
  with _ -> raise NoInput

(*Versione alternativa*)
let read_max' () =
  let rec aux m =
    try aux (max (read_int()) m)
    with _ -> m
  in aux (read_int())

(*La versione non funziona quando l'utente inserisce solo un intero. Perché?*)
let read_max'' () =
  let rec aux input m =
    try aux true (max (read_int()) m)
    with _ -> if input then m
              else raise NoInput
  in aux false (read_int())
;;

let read_max_min () =
  try let m = read_int()
      in let rec aux ma mi =
           try let n = read_int()
               in aux (max n ma) (min n mi)
           with _ -> (ma,mi)
         in aux m m
  with _ -> raise NoInput
;;

let rec tutti_minori n =
  try let m = read_int()
      in (tutti_minori n) && m<n
  with _ -> true

(*Versione alternativa*)
let rec tutti_minori' n =
  try let m = read_int()
      in match m<n with
           true -> tutti_minori' n
         | _ -> (tutti_minori' n) && false
  with _ -> true

(*La versione qui sotto non rispetta tutte le specifiche richieste. Quali? Perché?*)
let rec tutti_minori'' n =
  try let m = read_int()
      in match m<n with
           true -> tutti_minori'' n
         | _ -> false
  with _ -> true
;;

let rec occorre n =
  try let m = read_int()
      in  occorre n || m=n
  with _ -> false

(*Versione alternativa*)
let rec occorre' n =
  try let m = read_int()
      in match m=n with
           false -> occorre' n 
         | _ -> occorre' n || true
  with _ -> false

(*La versione qui sotto non rispetta tutte le specifiche rishieste. Quali? Perché?*)
let rec occorre'' n =
  try let m = read_int()
      in match m with
           n -> occorre'' n || true
         | _ -> occorre'' n 
  with _ -> false

(*La versione qui sotto non rispetta tutte le specifiche rishieste. Quali? Perché?*)
let rec occorre'' n =
  try let m = read_int()
      in match m=n with
           false -> occorre'' n
         | _ -> true
  with _ -> false
;;

let num_di_stringhe () = 
  let rec num_stringhe n =
    let s = read_line()
    in match s with
         "" -> n
       | _ -> num_stringhe (n+1)
  in num_stringhe 0
;;

let stringa_max () =
  let rec aux m s_max =
    let s = read_line()
    in let l = String.length s
       in if l=0 then s_max
               else if l > m then aux l s
          else aux m s_max
  in aux 0 ""
;;

let rec sumbetween m n =
  if m > n then 0
  else m + (sumbetween (m+1) n)

(*Versione iterativa*)
let sumbetween' m n =
  let rec aux m' n' sum =
    if m' > n' then sum
    else aux (m'+1) n' (m'+sum)
  in aux m n 0

(*Versione iterativa alternativa*)
let sumbetween'' m n =
  let rec aux m' sum =
    if m' > n then sum
    else aux (m'+1) (m'+sum)
  in aux m 0
;;

let sumto n =
  sumbetween 0 n
;;

let rec power n = function
    0 -> 1
  | k -> n * (power n (k-1))
  
(*Versione iterativa*)
let power' n k =
  let rec aux n' k' pow = match k' with
      0 -> pow
    | _ -> aux n' (k'-1) (n'*pow)
  in aux n k 1

(*Versione iteratiiva alternativa*)
let power'' n k =
  let rec aux pow = function
      0 -> pow
    | k' -> aux (n*pow) (k'-1) 
  in aux 1 k
;;

let rec fib  = function
    0 -> 0
  | 1 -> 1
  | n -> fib (n-1) + fib (n-2)

(*Versione iterativa*)
let fib' n =
  let rec aux acc1 acc2 = function
      0 -> acc2
    | 1 -> acc1
    | n' -> aux (acc1+acc2) acc1 (n'-1)
  in aux 1 0 n
;;

let maxstring s = 
  let rec aux s i cmax =
    try let c = s.[i]
        in if c > cmax then aux s (i+1) c
           else aux s (i+1) cmax
    with _ -> cmax
  in aux s 0 s.[0]

(*Versione alternativa*)
let maxstring' s = 
  let rec aux s i cmax =
    try aux s (i+1) (max s.[i] cmax)
    with _ -> cmax
  in aux s 0 s.[0]
;;
