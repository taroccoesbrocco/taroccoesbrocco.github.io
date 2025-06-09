(*La funzione seguente non è richiesta nell'esonero, però può essere utilizzata*)
let rec implode = function
    [] -> ""
  | x::xs -> (String.make 1 x)^(implode xs)
;;

let reverse_to_list s =
  let n = String.length s
  in let rec aux acc i s =
       if i=n then acc
       else aux (s.[i]::acc) (i+1) s
     in aux [] 0 s
;;

let reverse s = implode (reverse_to_list s)
;;

let palindrome s = (s = reverse s)
;;

let sumOddSquares l = List.fold_left (+) 0 (List.map (function x -> x*x) (List.filter (function x -> x mod 2 = 1) l))
;;

(*Versione alternativa*)
let sumOddSquares' l = List.fold_right (+) (List.map (function x -> x*x) (List.filter (function x -> x mod 2 = 1) l)) 0
;;

(*Versione alternativa*)
let sumOddSquares'' l =
  let odd x = x mod 2 = 1
  in let square x = x*x
     in List.fold_right (+) (List.map (square) (List.filter (odd) l)) 0
;;

let x = 1 ;;
let f y =
  function z -> x+y+z ;;
let x = 3 ;;
let w = (f 4) 6 ;;
(*Il tipo di f è int -> int -> int.

  Giustificazione:
  Infatti f è una funzione che prende due parametri y (esplicito) e z (implicito) e restituisce il valore di x+y+z. Si noti che x non è un parametro della funzoine ma una costante (il cui valore è aggiornato nell'ambiente globale). Quindi il tipo di f è della forma t1 -> t2 -> t3, dove t1 è il tipo di y, t2 è il tipo di z e t3 è il tipo di x+y+z. Si noti che la funzione (+) prende due parametri di tipo int e resituisce un valore di tipo int, pertanto x, y e z devono essere di tipo int e il risultato x+y+z è di tipo int. Dunque f è di tipo int -> int -> int.

  Il tipo di w è int e il suo valore è 11.

  Giustificazione:
  Il tipo di w è il tipo del valore che si ottiene applicando la funzione f (che è di tipo int -> int -> int) ai valori 4 e 6 (di tipo int), pertanto il tipo di w è int.

  Il valore di w è il valore che si ottiene applicando f a y=4 e z=6. Quando la funzione f viene definita, la variabile x ha valore 1 come memorizzato nell'ambiente globale. QuindiDunque la funzione f associa ai valori di y e z il valore di 1+y+z, pertanto nel caso y=4 e z=6 si ottiene che w ha il valore di (f 4)6 = 1+4+6=11.

  Il fatto che, dopo la definizione di f (ma prima di calcolare il valore di w), il valore di x venga aggiornato a 3 non ha alcun effetto nella definizione di f e quindi neanche nel valore di w. Infatti il la definizione di f fa riferimento al valore associato a x nell'ambiente locale nel momento della definizione, in questo caso x=1.

 *)
