let rec from i j k =
  if i > j then k
  else from i (j - 1) (j :: k)
(*Il tipo della funzione from è int -> int -> int list -> int list. Infatti, j è un int (a causa del -1); quindi anche i è un int (a causa della comparazione con j), mentre k è una int list (perché j viene aggiunto in testa a k). Il tipo restituito da from è int list perché è lo stesso tipo di k (a causa del ramo then).

  La funzione from applicata a i j k restituisce la lista di interi da i a j (inclusi) concatenata con la lista di interi k, cioè [i,...,j]@k.

  La funzione from è ricorsiva di coda (cioè iterativa) perché dopo ogni chiamata ricorsiva non viene eseguita alcuna altra operazione.
*)

let (--) i j =
  from i j []
(*Il tipo della funzione (--) è int -> int -> int list. Infatti from i j ha tipo int list -> int list, pertanto (--) i j, che è (from i j) applicata a [], ha tipo int list. Siccome gli argomenti i e j sono int, se ne deduce che (--) ha tipo int -> int -> int list.
 *)

let blabla = 0 -- 3
(*L'espressione blabla ha tipo int list e il suo valore è [0; 1; 2; 3]. Infatti, 0 -- 3 =
  = from 0 3 []
  = from 0 2 (3::[])
  = from 0 1 (2::3::[])
  = from 0 0 (1::2::3::[])
  = from 0 -1 (0::1::2::3::[])
  = (0::1::2::3::[]) = [1;2;3;4]
*)
;;


let pack l =
  let rec aux acc packing = function
      [] -> List.rev(packing::acc)
    | x::xs -> if List.length packing = 0 || x = List.hd packing then aux acc (x::packing) xs
               else aux (packing::acc) [x] xs
  in aux [] [] l

(*Versione alternativa*)
let pack' list =
  let rec aux current acc = function
    | [] -> []    (* Can only be reached if original list is empty *)
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
       if a = b then aux (a :: current) acc t
       else aux [] ((a :: current) :: acc) t  in
  List.rev (aux [] [] list)
;;


let f x = if x then x else x
(* f : bool -> bool *)

let g x y = if y then x else x
(* g : 'a -> bool -> 'a *)

let h x y z = if x then y else z
(* h : bool -> 'a -> 'a -> 'a *)

let i x y z = if x then y else y
(* i : bool -> 'a -> 'b -> 'a *)

let j x y = if y then x else (fun x y -> x) y
(* j : ('a -> bool) -> bool -> 'a -> bool poiché (fun x y -> x) : 'b -> 'a -> 'b è applicata a y : bool *)
;;
