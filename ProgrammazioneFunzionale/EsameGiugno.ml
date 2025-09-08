(*
  La funzione twice ha tipo ('a -> 'a) -> 'a -> 'a.

  Infatti twice riceve due parametri, f e x, quindi ha tipo 'b -> 'a -> 'c dove 'b è il tipo di f, 'a è i ltipo di x e 'c è il tipo di twice f a. Poiché f si applica x, allora f deve essere una funzione di tipo 'a -> 'd. Poiché f si applica a (f x), il tipo di fx deve essere a sua volta 'a, quindi f è di tipo 'b = 'a -> 'a. Di conseguenza, il tipo di f (f x) (e cioè di twice f a) è 'c = 'a.

  La funzione fourth è di tipo int -> int.

  Siccome fourth = twice square dove twice è di tipo ('a -> 'a) -> 'a -> 'a (si veda sopra) e square è di tipo int -> int (square riceve un parametro x e calcola la moltiplicazione intera di x per se stesso, quindi square ha tipo int -> int), allora fourth è l'applicazione parziale di twice a square che instanzia 'a con int, cioè una funzione di tipo int -> int, dove il parametro di fourth è il secondo parametro di square.


  Il tipo di what è int e il suo valore è 81.

  Infatti fourth è una funzione di tipo int -> int, per cui quando viene applicata all'intero 3 restituisce un intero. Il valore di tale intero è fourth 3 = twice square 3 = square (square 3) = square 9 = 81.
*)




let succ_opt = function
    None -> None
  | Some n -> Some (n+1)

let map_opt f = function
    None -> None
  | Some x -> Some (f x)

let max_opt = function
    [] -> None
  | x::xs -> let rec aux acc = function
             [] -> Some acc
               | y::ys -> if y>acc then aux y ys
                          else aux acc ys
             in aux x xs

(*Versione alternativa*)
let rec max_opt' = function
  | []   -> None
  | h::t -> match list_max t with
        | None   -> Some h
        | Some m -> Some (max h m)

(*Versione scorretta*)
let max_opt lst =
  let rec aux max = function
      [] -> if max=None then None else Some max
    | x::xs -> if max=None || x>max then (aux x xs) else aux max xs
  in aux None lst
;;



type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

exception NotFound

let t = Tr('a', Tr('f', Empty, Tr('c',Empty,Empty)),
          Tr('b', Tr('d',Empty,Empty), Tr('e',Empty,Empty)))

let search_path p t =
  let rec aux acc = function
      Empty -> raise NotFound
    | Tr(n,l,r) -> if p n then List.rev(n::acc)
                   else try aux (n::acc) l
                        with NotFound -> aux (n::acc) r
  in aux [] t

let path_to n k t =
  let path = search_path ((=)n) t
  in if List.length path = k then path
     else raise NotFound
;;


