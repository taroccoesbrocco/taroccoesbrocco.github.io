(*Grafo come lista di archi: orientati o non orientati a seconda che si usi la funzione successori o vicini, rispettivamente, per calcorare gli archi di un nodo*)
type 'a graph = ('a * 'a) list

let grafo1 = [(1,2); (1,3); (1,4); (2,6); (3,5); (4,6);
              (5,4); (6,5); (6,7)]
let grafo2 = [(1,2); (1,3); (1,4); (2,6); (3,5); (4,6);
              (5,4); (6,7); (6,5)]
let grafo3 = [(1,1); (1,2)]
let grafo4 = [(1,1); (1,2); (2,1)]
let grafo5 = [(1,2); (2,3); (3,2); (2,1)]
let grafo6 = [(1,1); (2,1)]
let grafo7 = [(1,2);(1,3);(3,4);(4,2)]

exception NotExistingNode ;;

(* successori : 'a -> 'a graph -> 'a list *)
let successori n g = List.map snd (List.filter (fun (n1,_) -> n=n1) g)

(*Elimina le ripetizioni in una lista: norepetition : 'a list -> 'a list *)
let rec norepetition = function
    [] -> []
  | x::xs -> x :: (norepetition (List.filter (fun y -> x<>y) xs))

(*Calcola i vicini di un nodo, considerando il grafo come non orientato, consente ripetizioni: vicini : 'a -> 'a graph -> 'a list *)
let vicini n g = List.map (function (n1,n2) -> if n=n1 then n2 else n1)
                    (List.filter (function (n1,n2) -> n=n1 || n=n2) g)

(*Calcola i vicini di un nodo senza ripetizioni, considerando il grafo come non orientato: vicini_norep : 'a -> 'a graph -> 'a list *) 
let vicini_norep n g = norepetition (vicini n g) 

(*Calcola i nodi di un grafo: nodes : 'a graph -> 'a list *)
let rec nodes g = norepetition(List.map fst g @ (List.map snd g))
;;

(*Ricerca di un nodo che soddisfi una proprietà e sia raggiungibile da un nodo di ingresso, secondo la visita in profondità: search_node : 'a graph -> 'a -> ('a -> bool) -> 'a *)  
let search_node graph start p =
  let rec search visited = function (* search : 'a list -> 'a list -> 'a *)
      [] -> raise NotExistingNode
    | n::rest -> if List.mem n visited then search visited rest (*non si passa per un nodo già visitato*)
                 else if p n then n
                 else search (n::visited) ((successori n graph) @ rest)
                             (* oppure: ((vicini n graph) @ rest) se il grafo non e’ orientato *)
  in search [] [start]

let test_connessi graph n m = try let _ = search_node graph n ((=) m) in true
                              with NotExistingNode -> false

let esistse_ciclo graph start = List.exists (fun x -> test_connessi graph x start) (successori start graph)
;;

(*Ricerca di un cammino senza cicli dal nodo di ingresso a un nodo che soddisfi una proprietà: search_path : 'a graph -> 'a -> ('a -> bool) -> 'a list *)
let search_path graph start p =
  let rec from_node visited n = (* from_node : 'a list -> 'a -> 'a list *)
    if List.mem n visited then raise Not_found (*cammino senza cicli non trovato*)
    else if p n then List.rev (n::visited) (*restituisce il cammino trovato*)
    else from_list (n::visited) (vicini n graph) (*n è visitato, visita i vicini di n*)
  and from_list visited = function (* from_list : 'a list -> 'a list -> 'a list *)
      [] -> raise Not_found 
    | n::rest -> try from_node visited n (*visita un vicino*)
                 with Not_found -> from_list visited rest (*bactrack per visitare gli altri vicini*)
  in from_node [] start

let ciclo graph start = let f = fun x -> try Some (search_path graph x ((=) start))
                                                with Not_found -> None
                        in let paths_some = List.map f (vicini start graph)
                           in  try start :: (value (List.hd(List.filter (fun x -> x<>None) paths_some)))
                               with _ -> failwith "There is no cycle."
;;
