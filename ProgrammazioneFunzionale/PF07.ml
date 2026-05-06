type direzione = Su | Giu | Destra | Sinistra
type posizione = int * int * direzione
type azione = Gira | Avanti of int

(*Selettori del tipo posizione*)
let pos_x (x,_,_) = x
let pos_y (_,y,_) = y
let pos_dir (_,_,dir) = dir

(*Selettori del tipo azione*)
let int_of_act = function
    Avanti n -> n
  | _ -> failwith "There is no integer with such an action"
;;

type posizione' = Pos of (int * int * direzione)

(*Selettori del tipo posizione'*)
let xcoord (Pos(x,_,_)) = x
let ycoord (Pos(_,y,_)) = y
let dir (Pos(_,_,d)) = d
;;

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
;;


type number = Int of int
            | Float of float

(* sum : number * number -> number *)
let sum = function
    (Int x,Int y) -> Int (x + y)
  | (Int x,Float y) -> Float ((float x) +. y)
  | (Float x,Int y) -> Float(x +. float y)
  | (Float x,Float y) -> Float (x +. y)

(*La versione seguente non funziona correttamente. Perché?*)
let sum' = function
    (Int x, Int y) -> Int (x + y)
  | (Int x, Float y) -> Float (x +. y)
  | (Float x, Int y) -> Float(x +. y)
  | (Float x, Float y) -> Float (x +. y)

let value  = function
    Int n -> float n
  | Float n -> n

(*La versione seguente non comipla. Perché?*)
let value' = function
    Int n -> n
  | Float n -> n
;;


type nat = Zero | Succ of nat

let rec int_of_nat = function
    Zero -> 0
  | Succ n -> succ(int_of_nat n)

let rec somma (n,m) = match n with
    Zero -> m
  | Succ k -> Succ(somma(k,m))

let rec nat_of_int  = function
    0 -> Zero
  | n when n>0 -> Succ(nat_of_int (n-1))
  | _ -> failwith "The input is not a natural number"

(*Versione alternativa*)
let rec nat_of_int  = function
    0 -> Zero
  | n -> if n>0 then Succ(nat_of_int (n-1))
         else failwith "The input is not a natural number"
;;


type 'a mylist = Nil | Cons of 'a * 'a mylist

let l0 = Nil
let l1 = Cons (1,Nil)
let l2 = Cons (1, Cons (2, Nil))
let l = Cons (1, Cons ('a', Nil))

(*Si confronti con*)
let l0' = []
let l1' = 1 :: []
let l2' = 1 :: (2 :: [])
let l' = 1 :: ('c' :: [])
;;


let rec new_assoc x = function
    [] -> None
  | (k,v)::rest -> if x=k then Some v
                   else new_assoc x rest

(*Versione alternativa*)
let rec new_assoc' x = function
    [] -> None
  | (k,v)::rest when k=x -> Some v
  | _::rest -> new_assoc' x rest

(*La versione seguente non funziona correttamente. Perché?*)
let rec new_assoc'' x = function
    [] -> None
  | (k,v)::rest when k=x -> Some v
  | rest -> new_assoc'' x rest

(*Esempio*)
let dizionario = [(0,"pippo"); (1,"pluto"); (2,"paperino")] ;;
List.map (function x -> new_assoc x dizionario) [1;5;3;0;4] ;;
List.map (function x -> List.assoc x dizionario) [1;5;3;0;4] ;;

let valore = function
    Some n -> n
  | None -> failwith "There is no value."

(* assoc_all dizionario chiavi = valori associati a chiavi in dizionario, ignorando le chiavi indefinite *)
let assoc_all dizionario chiavi =
  List.map valore (List.filter ((<>) None) (List.map (function x -> new_assoc x dizionario) chiavi))

(*Esempio*)
assoc_all dizionario [1;5;3;0;4] 
;;


