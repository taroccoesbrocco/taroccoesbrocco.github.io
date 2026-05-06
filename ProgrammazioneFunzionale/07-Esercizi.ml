type direzione = Su | Giu | Destra | Sinistra
type posizione = int * int * direzione
type azione = Gira | Avanti of int

(* gira : direzione -> direzione *)
let gira = function
    Su -> Destra
  | Giu -> Sinistra
  | Destra -> Giu
  | Sinistra -> Su

(* avanti : posizione -> int -> posizione *)
let avanti (x,y,dir) n =
  match dir with
    Su -> (x,y+n,dir)
  | Giu -> (x,y-n,dir)
  | Destra -> (x+n,y,dir)
  | Sinistra -> (x-n,y,dir)

(* sposta : posizione -> azione -> posizione *)
let sposta (x,y,dir) act =
  match act with
    Gira -> (x,y,gira dir) (*Le coordinate non cambiano, la direzione gira di 90 gradi in senso orario *)
  | Avanti n -> avanti (x,y,dir) n

let esegui p al = List.fold_left sposta p al
;;


type nat = Zero | Succ of nat

let rec int_of_nat = function
    Zero -> 0
  | Succ n -> (int_of_nat n) + 1

let rec somma m = function
    Zero -> m
  | Succ k -> Succ(somma m k)

(*La versione seguente non è corretta. Perché?*)
let rec somma' m = function
    Zero -> m
  | Succ k -> succ(somma' m k)

let rec prod n = function
    Zero -> Zero
  | Succ k -> somma n (prod n k)
;;


type chiave = Aperta | Chiusa
type cassaforte = chiave list

let giraPrima cl =
  try match List.hd cl with
        Aperta -> Chiusa::(List.tl cl)
      | _ -> Aperta::(List.tl cl)
  with _ -> failwith "There's no key!"

let rec giraDopoChiusa = function
    [] -> failwith "There's no key!"
  | Aperta::cl -> Aperta::(giraDopoChiusa cl)
  | Chiusa::cl -> Chiusa::(giraPrima cl)

let successori cl =
  try (giraDopoChiusa cl)::[giraPrima cl]
  with _ -> [giraPrima cl]
;;


type obj = Miss | Cann | Barca
type situazione = obj list * obj list

let initial = ([Miss;Miss;Miss;Cann;Cann;Cann;Barca], [])

type azione = From_left of obj list
            | From_right of obj list

let safe sit =
  let rec safe_side miss cann = function
      [] -> miss >= cann || miss=0
    | Cann::xs -> safe_side miss (cann+1) xs
    | Miss::xs -> safe_side (miss+1) cann xs
    | Barca::xs -> safe_side miss cann xs
  in safe_side 0 0 (fst sit) && (safe_side 0 0 (snd sit))

(*doable : obj list-> obj list -> bool*)
let doable side act = if not (List.mem Barca side) then failwith "The ship is not on the side"
                      else if List.length act > 2 then failwith "The ship is too charged"
                      else if List.length (List.filter ((=) Miss) act) > List.length (List.filter ((=) Miss) side) then failwith "There are not enough missioners"
                      else if List.length (List.filter ((=) Cann) act) > List.length (List.filter ((=) Cann) side) then failwith "There are not enough cannibals"
                      else true

(*multisetdiff : 'a list -> 'a list -> 'a list*)
let multisetdiff l1 l2 =
  let l1' = List.sort compare l1 (*l1' and l2' are sorted with an increasing order*)
    in let l2' = List.sort compare l2
       in let rec aux l = function
              [] -> l
            | y::ys as l' -> match l with
                               [] -> []
                             | x::xs -> if x=y then aux xs ys
                                        else if x<y then x :: (aux xs l') (*x remains, because l' is sorted*)
                                        else aux l ys (*y is useless because l is sorted*)
          in aux l1' l2'

(*Versione alternativa*)
let multisetdiff' l1 l2 =
  let l1' = List.sort compare l1 (*l1' and l2' are sorted with an increasing order*)
    in let l2' = List.sort compare l2
       in let rec aux l l' = match (l,l') with
              (l, []) -> l 
            | ([], _) -> [] 
            | (x::xs, y::ys) -> if x=y then aux xs ys
                                else if x<y then x :: (aux xs l') (*x remains, because l' is sorted*)
                                else aux l ys (*y is useless because l is sorted*)
          in aux l1' l2'

(*Versione iterativa*)
let multisetdiff'' l1 l2 =
  let l1' = List.sort compare l1 (*l1' and l2' are sorted with an increasing order*)
    in let l2' = List.sort compare l2
       in let rec aux acc l l' = match (l,l') with
              (l, []) -> acc@l
            | ([], _) -> acc 
            | (x::xs, y::ys) -> if x=y then aux acc xs ys
                                else if x<y then aux (x::acc) xs l' (*x remains, because l' is sorted*)
                                else aux acc l ys (*y is useless because l is sorted*)
          in aux [] l1' l2'
                
let applica act sit = match act with
    From_left l -> let new_sit = (multisetdiff (fst sit) (Barca::l), Barca::l@(snd sit))
                   in if doable (fst sit) l && safe new_sit then new_sit
                      else failwith "The new situation is unsafe"
  | From_right l -> let new_sit = (Barca::l@(fst sit), multisetdiff (snd sit) (Barca::l))
                    in if doable (snd sit) l && safe new_sit then new_sit
                       else failwith "The new situation is unsafe"

let actions =
  let elems =
    [[Miss];[Cann];[Miss;Cann];[Miss;Miss];[Cann;Cann]]
  in (List.map (function x -> From_left x) elems)
     @ (List.map (function x -> From_right x) elems)

let value = function
    Some v -> v
  | None -> raise Not_found

let from_sit sit = 
  let aux x = try Some (applica x sit)
              with _ -> None
  in List.map value (List.filter ((<>) None) (List.map aux actions))
;;


type 'a pattern = Jolly | Val of 'a

let rec most_general_match l1 l2 = match (l1,l2) with
    ([],[]) -> []
  | (_::xs,[]) | ([],_::xs) -> failwith "The two lists do not have the same length"
  | (x::xs,y::ys) -> if x=y then (Val x)::(most_general_match xs ys)
                     else Jolly::(most_general_match xs ys)

(*Versione alternativa*)
let rec zip l1 l2 = match (l1,l2) with
    ([],[]) -> []
  | (_::xs,[]) | ([],_::xs) -> failwith "The two lists do not have the same length"
  | (x::xs,y::ys) -> (x,y)::(zip xs ys)

let most_general_match' l1 l2 = List.map (function (x,y) -> if x=y then (Val x) else Jolly) (zip l1 l2)
;;
