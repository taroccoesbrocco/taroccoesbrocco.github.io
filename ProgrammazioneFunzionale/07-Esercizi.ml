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
      [] -> miss >= cann
    | Cann::xs -> safe_side miss (cann+1) xs
    | Miss::xs -> safe_side (miss+1) cann xs
    | Barca::xs -> safe_side miss cann xs
  in safe_side 0 0 (fst sit) && (safe_side 0 0 (snd sit))
;;


type 'a pattern = Jolly | Val of 'a

let rec most_general_match l1 l2 = match (l1,l2) with
    ([],[]) -> []
  | (_::xs,[]) | ([],_::xs) -> failwith "The two lists do not have the same length"
  | (x::xs,y::ys) -> if x=y then (Val x)::(most_general_match xs ys)
                     else Jolly::(most_general_match xs ys)
;;

(*Versione alternativa*)
let rec zip l1 l2 = match (l1,l2) with
    ([],[]) -> []
  | (_::xs,[]) | ([],_::xs) -> failwith "The two lists do not have the same length"
  | (x::xs,y::ys) -> (x,y)::(zip xs ys)

let most_general_match' l1 l2 = List.map (function (x,y) -> if x=y then (Val x) else Jolly) (zip l1 l2)
;;
