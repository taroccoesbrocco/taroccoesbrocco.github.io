type expr =
  Int of int
| Var of string
| Sum of expr * expr
| Diff of expr * expr
| Mult of expr * expr
| Div of expr * expr

type ambiente = (string * int) list

let rec eval env = function
    Int n -> n
  | Var x -> List.assoc x env
  | Sum (e1,e2) -> eval env e1 + (eval env e2)
  | Diff (e1,e2) -> eval env e1 - (eval env e2)
  | Mult (e1,e2) -> eval env e1 * (eval env e2)
  | Div (e1,e2) -> eval env e1 / (eval env e2)
;;


type ’a tree = Empty | Tr of ’a * ’a tree * ’a tree

let is_empty = function
    Empty -> true
  | _ -> false

let is_leaf = function
    Tr(_,Empty,Empty) -> true
  | _ -> false

exception EmptyTree

let root = function
    Empty -> raise EmptyTree
  | Tr(x,_,_) -> x

let left = function
    Empty -> raise EmptyTree
  | Tr(_,t,_) -> t

let right = function
    Empty -> raise EmptyTree
  | Tr(_,_,t) -> t

let rec size = function
    Empty -> 0
  | Tr(_,t1,t2) -> 1 + (size t1) + (size t2)

(*Versione alternativa*)
let rec size' t =
  if is_empty t then 0
  else 1 + size' (left t) + size' (right t)

let rec height = function
    Empty -> -1
  | Tr(_,t1,t2) -> 1 + max (height t1) (height t2)

(*Versione alternativa*)
let rec height' t =
  if is_empty t then -1
  else 1 + max (height' (left t)) (height' (right t))
;;

let rec add x = function
    [] -> [(x,1)]
  | (y,n)::ys -> if y=x then (y,n+1)::ys
                 else (y,n)::(add x ys)

let count t =
  let rec aux result = function
      Empty -> result
    | Tr(x,left,right) ->  aux (aux (add x result) left) right  
  in aux [] t
;;

let t = Tr("A",
           Tr("B",
              Tr("A",Empty,Empty),
              Tr("D",Empty,Empty)
             ),
           Tr("D",
              Tr("D",Empty,Empty),
              Tr("A",Empty,Empty)
             )
          )
;;

let alphabet = Tr('*',
                  Tr('E',
                     Tr('I',
                        Tr('S',
                           Tr('H',Empty,Empty),
                           Tr('V',Empty,Empty)
                          ),
                        Tr('U',
                           Tr('F',Empty,Empty),
                           Empty
                          )
                       ),
                     Tr('A',
                        Tr('R',
                           Tr('L',Empty,Empty),
                           Empty
                          ),
                        Tr('W',
                           Tr('P',Empty,Empty),
                           Tr('J',Empty,Empty)
                          )
                       )
                    ),
                  Tr('T',
                     Tr('N',
                        Tr('D',
                           Tr('B',Empty,Empty),
                           Tr('X',Empty,Empty)
                          ),
                        Tr('K',
                           Tr('C',Empty,Empty),
                           Tr('Y',Empty,Empty)
                          )
                       ),
                     Tr('M',
                        Tr('G',
                           Tr('Z',Empty,Empty),
                           Tr('Q',Empty,Empty)
                          ),
                        Tr('O',Empty,Empty)
                       )
                    )
                 )

exception NotFound

let morse c =
  let rec aux code = function
      Empty -> raise NotFound
    | Tr(c',left,right) -> if c=c' then code
                           else try aux (code^".") left
                                with _ -> aux (code^"-") right
  in aux "" alphabet
;;

(* treeprint : (’a -> ’b) -> ’a tree -> unit *)
(* treeprint print t = stampa, con opportuna indentazione, l’albero t,
utilizzando la funzione print per la stampa dei nodi *)
(* aux: string -> ’a tree -> unit *)
(* aux ind t stampa l’albero t, premettendo ad ogni riga un certo
numero di spazi seguiti dalla stringa ind.
Il numero di spazi viene incrementato per la stampa di
ciascun sottoalbero *)
let treeprint print t =
  let rec aux ind = function
      Empty -> print_string (ind ^ "Empty")
    | Tr(x,Empty,Empty) ->
       begin
         print_string (ind^"Tr(");
         print x;
         print_string ",Empty,Empty)"
       end
    | Tr(x,t1,t2) ->
       begin
         print_string (ind^"Tr(");
         print x;
         print_string ",\n";
         aux ("  "^ind) t1;
         print_string ",\n";
         aux ("  "^ind) t2;
         print_string ")"
       end
  in aux "" t ;
     print_string "\n"         
let treeprint print t =
  let rec aux ind = function
      Empty -> print_string (ind ^ "Empty")
    | Tr(x,Empty,Empty) ->
       begin
         print_string (ind^"Tr(");
         print x;
         print_string ",Empty,Empty)"
       end
    | Tr(x,t1,t2) ->
       begin
         print_string (ind^"Tr(");
         print x;
         print_string ",\n";
         aux ("  "^ind) t1;
         print_string ",\n";
         aux ("  "^ind) t2;
         print_string ")"
       end
  in aux "" t ;
     print_string "\n"         
;;
