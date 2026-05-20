type 'a ntree = Tr of 'a * 'a ntree list

let leaf x = Tr(x,[])

let t = Tr(1,
           [Tr(2,
                 [Tr(3,[leaf 4; leaf 5]);
                  Tr(6,[leaf 7]);
                  leaf 8]);
            leaf 9;
            Tr(10,
               [Tr(11,
                   [leaf 12; leaf 13; leaf 14]);
                leaf 15;
                Tr(16,[leaf 17;
                       Tr(18,[leaf 19; leaf 20])])])])
;;

type player = Min | Max
type minmaxtree = Leaf of int
                | Node of (player * int) * minmaxtree list

let tminimax = Node((Max,0),
                    [Node((Min,100),[Leaf 3; Leaf 12; Leaf 8]);
                     Node((Min,0),[Leaf 2; Leaf 4; Leaf 6]);
                     Node((Min,30),[Leaf 14; Leaf 15; Leaf 10])]
                 )

(* min_minmaxlist : minmaxtree list -> int *)
(* min_minimaxlist l restituisce il valore minimo tra le radici degli alberi minmax nella lista l *)
let min_minimaxlist l = 
  let rec aux mini = function (* aux : int -> minimaxtree list -> int *)
        [] -> mini
      | (Leaf n)::xs | Node((_,n),_)::xs -> if n<mini then aux n xs
                                              else aux mini xs
    in let start = match l with
           [] -> raise Not_found
         | (Leaf n)::xs | Node((_,n),_)::xs -> n
       in aux start l

(* max_minmaxlist : minmaxtree list -> int *)
(* max_minimaxlist l restituisce il valore massimo tra le radici degli alberi minmax nella lista l *)
let max_minimaxlist l = 
    let rec aux maxi = function (* aux : int -> minimaxtree list -> int *)
        [] -> maxi
      | (Leaf n)::xs | Node((_,n),_)::xs -> if n>maxi then aux n xs
                                              else aux maxi xs
    in let start = match l with
           [] -> raise Not_found
         | (Leaf n)::xs | Node((_,n),_)::xs -> n
       in aux start l
                          
let rec propagate = function
    Leaf n -> Leaf n
  | Node ((Min,_),l) -> let new_l = List.map propagate l in
                        Node((Min, min_minimaxlist new_l), new_l)
  | Node ((Max,_),l) -> let new_l = List.map propagate l in
                        Node((Max, max_minimaxlist new_l), new_l)
;;
