exception BadTime
;;

let somma_ore (h1,m1) (h2,m2) =
  if (0<=h1 && h1<24) && (0<=h2 && h2<24) && (0<=m1 && m1<60) && (0<=m2 && m2<60) then
    let m = m1 + m2
    in ((h1 + h2 + m/60) mod 24, m mod 60)
  else raise BadTime
;;

exception NoInput 
;;

let read_max () =
  try let m = read_int()
      in let rec aux m' =
           try let n = read_int()
               in aux (max n m')
           with _ -> m'
         in aux m 
  with _ -> raise NoInput
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
;;

(*Versione alternativa*)
let rec tutti_minori' n =
  try let m = read_int()
      in match m<n with
           true -> tutti_minori' n
         | _ -> (tutti_minori' n) && false
  with _ -> true
;;

(*La versione qui sotto non rispetta tutte le specifiche richieste. Quali? Perché?*)
let rec tutti_minori' n =
  try let m = read_int()
      in match m<n with
           true -> tutti_minori n
         | _ -> false
  with _ -> true
;;

let rec occorre n =
  try let m = read_int()
      in  occorre n || m=n
  with _ -> false
;;

(*Versione alternativa*)
let rec occorre' n =
  try let m = read_int()
      in match m=n with
           false -> occorre' n 
         | _ -> occorre' n || true
  with _ -> false
;;

(*La versione qui sotto non rispetta tutte le specifiche rishieste. Quali? Perché?*)
let rec occorre' n =
  try let m = read_int()
      in match m with
           n -> occorre' n || true
         | _ -> occorre' n 
  with _ -> false
;;

(*La versione qui sotto non rispetta tutte le specifiche rishieste. Quali? Perché?*)
let rec occorre' n =
  try let m = read_int()
      in match m=n with
           false -> occorre' n
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

let sumbetween m n =
  let rec aux m n sum =
    if m > n then sum
    else aux (m+1) n (m+sum)
  in aux m n 0
;;

let sumto n =
  sumbetween 0 n
;;

let power n k =
  let rec aux n k pow =
    match k with
      0 -> pow
    | _ -> aux n (k-1) (n*pow)
  in aux n k 1
;;

let rec fib  = function
    0 -> 0
  | 1 -> 1
  | n -> fib (n-1) + fib (n-2)
;;

let maxstring s = 
  let rec aux s i cmax =
    try let c = s.[i]
        in if c > cmax then aux s (i+1) c
           else aux s (i+1) cmax
    with _ -> cmax
  in aux s 0 s.[0]
;;

let maxstring' s = 
  let rec aux s i cmax =
    try aux s (i+1) (max s.[i] cmax)
    with _ -> cmax
  in aux s 0 s.[0]
;;
