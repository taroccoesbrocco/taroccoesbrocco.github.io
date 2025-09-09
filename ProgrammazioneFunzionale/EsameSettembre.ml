let (@@) g f x = g (f x)

(*
  L'espressione g @@ f ha tipo 'a -> 'c. Infatti, tale espressione è la notazione infissa dell'espressione (@@) g f, cioè l'applicazione della funzione (@@) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c applicata alle funzioni g : 'b -> 'c e f : 'a -> b'.

  Il valore restituito da (String.length @@ string_of_int) 100 è 3. Infatti, String.length @@ string_of_int è una funzione di tipo int -> int che computa il numero di caratteri (String.length) nella stringa di un intero (string_of_int) dato in argomento. 
 *)

(*
  L'espressione List.map g (List.map f lst) ha tipo 'c list. Infatti, l'espressione List.map f lst ha tipo 'b list, pertanto applicare List.map a g e List.map produce una lista di tipo 'c.
 *)

(*
  List.map g (List.map f lst) = List.map (g @@ f) lst
 *)


type date = int*int*int

exception NoComparison

let date (d,m,y) =
  let bissextile = (y mod 4 = 0) && (y mod 100 <> 0 || y mod 400 = 0) 
  in if m<1 || m>12 || d<1 then false
     else if ((m=1 || m=3 || m=5 || m=7 || m=8 || m=10 || m=12) && d>31) || ((m=4 || m=6 || m=9 || m=11) && d>30) || (bissextile && m=2 && d>29) || (not bissextile && m=2 && d>28) then false
     else true

(*Versione alternativa*)
let date (d,m,y) =
  let bissextile = (y mod 4 = 0) && (y mod 100 <> 0 || y mod 400 = 0) 
  in if m<1 || m>12 || d<1 then false
     else not ((m=1 || m=3 || m=5 || m=7 || m=8 || m=10 || m=12) && d>31) || ((m=4 || m=6 || m=9 || m=11) && d>30) || (bissextile && m=2 && d>29) || (not bissextile && m=2 && d>28)


let is_after (d1,m1,y1) (d2,m2,y2) =
  if not ((date (d1,m1,y1)) && (date (d2,m2,y2))) then raise NoComparison
  else if (d1,m1,y1) = (d2,m2,y2) then 0
  else if y1 < y1 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2) then (-1)
  else 1

(*Versione alternativa*)
let is_after dt1 dt2 =
  if (date dt1  && date dt2) then
    if dt1 = dt2 then 0
    else match (dt1, dt2) with
           ((d1,m1,y1), (d2,m2,y2)) ->
           if y1 < y1 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2) then (-1)
           else 1
  else raise NoComparison


let rec earliest = function
    [] -> None
  | dt::l -> match earliest l with
               None -> Some dt
             | Some dt' -> Some (if is_after dt dt' = (-1) then dt else dt')

(*Versione alternativa*)
let earliest' = function
    [] -> None
  | dt::lst -> Some (List.hd(List.sort is_after dt::lst))

(*Versione alternativa*)
let earliest'' lst  = try Some (List.hd(List.sort is_after lst))
                 with _ -> None
;;

(* Se lst è una lista con almeno una data non valida, allora l'espressione earliest lst (o earliest' lst o earliest'' lst) restituisce l'eccezione NoComparison (sollevata da is_after), a meno che lst non sia di lunghezza 1 (cioè il suo unico elemento è una data non valida), in qual caso l'espressione restituisce l'unica data.
 *)
