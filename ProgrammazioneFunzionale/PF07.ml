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


type nat = Zero | Succ of nat

let rec int_of_nat = function
    Zero -> 0
  | Succ n -> succ(int_of_nat n)

let rec somma (n,m) = match n with
    Zero -> m
  | Succ k -> Succ(somma(k,m))
;;


let rec new_assoc x = function
    [] -> None
  | (k,v)::rest -> if x=k then Some v
                   else new_assoc x rest

(*Esempio*)
let alist = [(0,"pippo"); (1,"pluto"); (2,"paperino")] ;;
List.map (function x -> new_assoc x alist) [1;5;3;0;4] ;;
List.map (function x -> List.assoc x alist) [1;5;3;0;4] ;;

let valore = function
    Some n -> n
  | None -> failwith "There is no value."

(* assoc_all lista chiavi = valori associati a chiavi in lista,
ignorando le chiavi indefinite *)
let assoc_all lista chiavi =
  List.map valore (List.filter ((<>) None) (List.map (function x -> new_assoc x lista) chiavi))
;;
let rec new_assoc x = function
    [] -> None
  | (k,v)::rest -> if x=k then Some v
                   else new_assoc x rest

(*Esempio*)
let alist = [(0,"pippo"); (1,"pluto"); (2,"paperino")] ;;
List.map (function x -> new_assoc x alist) [1;5;3;0;4] ;;
List.map (function x -> List.assoc x alist) [1;5;3;0;4] ;;

let valore = function
    Some n -> n
  | None -> failwith "There is no value."

(* assoc_all lista chiavi = valori associati a chiavi in lista,
ignorando le chiavi indefinite *)
let assoc_all dizio chiavi =
  List.map valore (List.filter ((<>) None) (List.map (function x -> new_assoc x dizio) chiavi))
;;

