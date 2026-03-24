let pi = 3.14159;;
let area x = pi *. x;;
let pi = 0.0;;
let x = "pippo";;
let a = area 3.0 ;;

(* La risposta di OCaml è
   val a : float = 9.42476999999999876
   perché a è il risultato dell'applicazione della funzione area : float -> float all'argomento 3.0 (un float) per cui a è un oggetto di tipo float il cui valore è 3.14159*3.0 = 9.42476999999999876. Si noti che:
   - nel calcolo di a, il valore di pi utilizzato è quello nella definizione di area, che è pari al valore di pi nell'ambiente al momento di definire area, cioè pi = 3.14159 e non pi = 0.0;
   - nel calcolo di a, si utilizza la variabile x come parametro della funzione area, pertanto x è una variabile muta che non ricerca il suo valore nell'ambiente, ma nell'argomento passato a area (3.0).
 *)


let y = 100 ;;
let x = 5 ;;
let h x = y+x ;;
let y = 0 ;;
h 7 ;;

(* h 7 è un oggetto di tipo int che vale 107. Infatti:
   - h è una funzione int -> int che prende un argomento x e restituisce il valore di x+100 (100 è il valore della variabile y nell'ambiente quando viene definita la funzione h)
   - 7 è un int che viene passato come argomento alla funzione h, pertanto h 7 è di tipo int e il suo valore è 7+100=107.
   Si noti che:
   - nel calcolo di h 7, il valore di y utilizzato è quello che nella definizione di h, che è pari al valore di y nell'ambiente al momento di definire h, cioè y = 100 e non y = 0;
   - nel calcolo di h 7, si utilizza la variabile x come parametro della funzione h, pertanto x è una variabile muta che non ricerca il suo valore nell'ambiente, ma nell'argomento passato a area (7).
*)


let punct x = x = '.' || x = ',' || x = ';' ;;
punct ;;

(* punct è una funzione di tipo char -> bool, il cui pseudo-valore è <fun>, come ogni altra funzione. 
   Infatti punct ha un parametro x che è di tipo char perché viene confrontato con i caratteri '.' o ',' o ';'. Il valore restituito da punct è un bool perché è il risultato dell'applicazione dell'operatore || (or) che restituisce un bool.
 *)


let quadrupla =
  (5,('c',"antonio",(),if 3>4 then 0 else 1),"pippo",true) ;;
let pi1 (x,y,z,w) = x ;;
let pi2 (x,y,z,w) = y ;;
let pi3 (x,y,z,w) = z ;;
let pi4 (x,y,z,w) = w ;;
pi3 (pi2 quadrupla) ;;
pi4 (pi2 quadrupla) ;;

(* I tipi delle funzioni sono i seguenti:
   - pi1 : 'a*'b*'c*'d -> 'a
   - pi2 : 'a*'b*'c*'d -> 'b
   - pi3 : 'a*'b*'c*'d -> 'c
   - pi4 : 'a*'b*'c*'d -> 'd
   Nessuna di queste funzioni può essere applicata a una quintupla perché si aspettano una quadrupla come argomento.

   pi2 quadrupla è un oggetto di tipo char*string*unit*int che vale ('c', "antonio", (), 1). Pertanto:
   - pi3 (pi2 quadrupla) è un oggetto di tipo unit che vale ();
   - pi4 (pi2 quadrupla) è un oggetto di tipo int che vale 1 (poiché 3>4 vale false quindi l'espressione if-then-else prende il valore del ramo else).
 *)


if E then true else false ;;
E ;;

if E then false else true ;;
not E ;;

if E then F else false ;;
E && F ;;

if E then F else true ;;
(not E) || F ;;

if E then true else F ;;
E || F ;;

if E then false else F ;;
(not E) && F ;;

(* Un modo per verificare la correttezza delle risposte qui sopra è sviluppare le tavole di verità delle espressioni qui sopra, verificando che l'espressione if-then-else abbia la stessa tavola di verità dell'espressione con gli operatori booleani.
 *)


let square x = x*x ;;
let twice f x = f (f x) ;;
let fourth = twice square ;;
let what = fourth 3 ;;

(* La funzione square ha tipo int -> int mentre la funzione  twice ha tipo ('a -> 'a) -> 'a -> 'a. Lo pseudo-valore di entrambe le funzioni è <fun>, come ogni funzione. Infatti:
   - square riceve un parametro x, quindi ha tipo 'a -> 'b dove 'a è il tipo di x e 'b è il tipo di x*x. Poiché * ha tipo int -> int -> int, il tipo di x (cioè 'a) deve essere int, e il tipo restituito da square (cioè 'b) deve essere il tipo di x*x, che int.
   - twice riceve due parametri, f e x, quindi ha tipo 'b -> 'a -> 'c dove 'b è il tipo di f, 'a è il tipo di x e 'c è il tipo di twice f a. Poiché f si applica x, allora f deve essere una funzione di tipo 'a -> 'd. Poiché f si applica a (f x), il tipo di fx deve essere a sua volta 'a, quindi f è di tipo 'b = 'a -> 'a. Di conseguenza, il tipo di f (f x) (e cioè di twice f a) è 'c = 'a.

  La funzione fourth è di tipo int -> int, mentre lo pseudo-valore di fourth è <fun>, come ogni funzione. Infatti, Siccome fourth = twice square dove twice è di tipo ('a -> 'a) -> 'a -> 'a (si veda sopra) e square è di tipo int -> int (square riceve un parametro x e calcola la moltiplicazione intera di x per se stesso, quindi square ha tipo int -> int), allora fourth è l'applicazione parziale di twice a square che instanzia 'a con int, cioè una funzione di tipo int -> int, dove il parametro di fourth è il secondo parametro di square.

  Il tipo di what è int e il suo valore è 81. Infatti fourth è una funzione di tipo int -> int, per cui quando viene applicata all'intero 3 restituisce un intero. Il valore di tale intero è fourth 3 = twice square 3 = square (square 3) = square 9 = 81.

*)


fun x -> x ;;
let rec fix x = x (fix x) ;;
let rec fix = fun x -> x (fix x);;
fix 5 ;;
fix (fun x -> x) ;;
if 5 < 0 then fix (fun x -> x) else 7 ;;

(* fun x -> x è una funzione (anonima) di tipo 'a -> 'a il cui pseudo-valore è <fun>, come ogni funzione.

   fix è una funzione di tipo ('a -> 'a) -> 'a, pertanto il suo lo pseudo-valore è <fun>, come ogni funzione. Infatti, fix riceve un parametro x e restituisce il valore di x(fix x), quindi fix è una funzione il cui tipo è della forma 'b -> 'a dove 'b è i ltipo del parametro x e 'a è il tipo restituito da fix. Poiché x viene applicato a (fix x), x deve essere a sua volta una funzione di un argomento il cui input è uguale al tipo 'a  restituito da fix (siccome (fix x) è l'argomento di x) e il cui output è uguale al tipo 'a restituito da fix (siccome il valore restituito da fix è il risultato dell'applicazione di x al suo argomento), dunque il tipo di x è 'b = 'a -> 'a.
   
   fix 5 dà un errore perché fix è una funzione di tipo ('a -> 'a) -> 'a, però il suo argomento è 5 che è di tipo int. Ora, non c'è modo di far combaciare il tipo 'a -> 'a del paramentro di fix con il tipo dell'argomento 5 che è int.

   Il tipo di fix (fun x -> x) è 'a perché fix è una funzione di tipo ('a -> 'a) -> 'a mentre il suo argomento fun x -> x è di tipo 'a -> 'a. Tuttavia fix (fun x -> x) non ha alciun valore perché l'esecuzione del programma non termina: infatti,
   fix (fun x -> x) = (fun x -> x) (fix (fun x -> x))
                    = (fun x -> x) ((fun x -> x) (fix (fun x -> x)))
                    = (fun x -> x) ((fun x -> x) ((fun x -> x) (fix (fun x -> x))))
                    = ...

   L'espressione if 5 < 0 then fix (fun x -> x) else 7 ha tipo int e vale 7. Infatti, il tipo del ramo then è il tipo di fix (fun x -> x) che è 'a (si veda sopra), mentre il tipo del ramo else è il tipo di 7 che è int. Il tipo più generale compatibile con i due rami dell'if-then-else è il tipo più generale compatibile con 'a e con int, che è int. Pertnato, il tipo dell'espressione if 5 < 0 then fix (fun x -> x) else 7 è int. 
   Il valore di 5<0 è false, dunque l'espressione if 5 < 0 then fix (fun x -> x) else 7 prende il valore del ramo else, che è 7.
*)


if true then fst else snd ;;
function (x,y) -> x ;;
fst = function (x,y) -> x ;;
snd = function (x,y) -> x ;;

(* L'espressione if true then fst else snd è una funzione di tipo 'a*'a -> 'a, pertanto il suo pseudo-valore è <fun>. Infatti, il tipo del ramo then è il tipo della funzione fst che è 'a*'b -> 'a, mentre il tipo del ramo else è snd che è 'a*'b -> 'b. Siccome i due rami devono avere lo stesso tipo, il tipo più generale che combacia con i tipi dei rami then e else è 'a*'a -> 'a, che è quindi il tipo di if true then fst else snd. Il valore di if true then fst else snd è il valore del ramo then (siccome la condizione è vera), ossia della funzione fst, che è quindi lo pseudo-valore <fun>. 

   L'espressione function (x,y) -> x è una funzione (anonima) di tipo 'a*'b -> 'a, pertanto il suo pseudo-valore è <fun>.

   L'espressione fst = function (x,y) -> x ha tipo bool perché sia fst sia function (x,y) -> x hanno tipo 'a*'b -> 'a, quindi la loro comparazione è ammissibile e restituisce il tipo bool. Tuttavia l'espressione non ha alcun valore perché lo pseudo-valore sia di fst sia di function (x,y) -> x è <fun> che non può essere comparato con nessuno, neanche con se stesso, quindi il test dell'uguaglianza restituisce un errore.

   L'espressione snd = function (x,y) -> x ha tipo bool perché snd ha tipo 'a*'b -> 'b e function (x,y) -> x hanno tipo 'a*'b -> 'a, quindi il tipo più generale compatibile tra i due è 'a*'a -> 'a e di conseguenza la loro comparazione è ammissibile e restituisce il tipo bool. Tuttavia l'espressione non ha alcun valore perché lo pseudo-valore sia di snd sia di function (x,y) -> x è <fun> che non può essere comparato con nessuno, neanche con se stesso, quindi il test dell'uguaglianza restituisce un errore.
*)
