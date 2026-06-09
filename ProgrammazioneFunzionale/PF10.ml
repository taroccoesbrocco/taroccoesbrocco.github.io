(*Grafo come lista delle coppie: (nodo, lista dei suoi successori)*)
type 'a graph = ('a * 'a list) list

exception NotExistingNode

let grafo = [(1,[2;3;4]);
             (2,[6]);
             (3,[5]);
             (4,[6]);
             (5,[4]);
             (6,[5;7]);
             (7,[])
            ]

let rec successori n = function
    [] -> raise NotExistingNode
  | (m,ns)::g -> if n=m then ns
                 else successori n g

(*Versione alternativa*)
let rec successori' n = function
    [] -> raise NotExistingNode
  | (m,ns)::g when m=n -> ns
  | (_,_)::g -> successori' n g

(*Versione alternativa*)
let successori'' n g =
  try List.assoc n g
  with Not_found -> raise NotExistingNode

(*Calcola i nodi di un grafo: nodi : 'a graph -> 'a list *)
let nodes g = List.map fst g
;;


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


(*Non distingue pozzi dai nodi assenti dal grafo*)
let rec successori n = function
    [] -> []
  | (n1,n2)::g -> if n=n1 then n2::(successori n g)
                  else successori n g

(*Versione alternativa: non distingue pozzi dai nodi assenti dal grafo*)
let successori' n g = List.map snd (List.filter (fun (n1,_) -> n=n1) g)

(*Versione alternativa: distingue pozzi dai nodi assenti dal grafo*)
let successori'' n g = if List.exists (fun (x,y) -> x=n || y=n) g
                       then List.map snd (List.filter (fun (n1,_) -> n=n1) g)
                       else raise NotExistingNode
;;

(*Calcola i vicini di un nodo, considerando il grafo come non orientato, consente ripetizioni*) 
let rec vicini n = function
    [] -> []
  | (n1,n2)::g -> if n=n1 then n2::(vicini n g)
                  else if n=n2 then n1::(vicini n g)
                  else vicini n g

(*Versione alternativa: considera il grafo come non orientato, consente ripetizioni*)
let vicini' n g = List.map (function (n1,n2) -> if n=n1 then n2 else n1)
                    (List.filter (function (n1,n2) -> n=n1 || n=n2) g)
;;

(*Elimina le ripetizioni in una lista: norepetition : 'a list -> 'a list *)
let rec norepetition = function
    [] -> []
  | x::xs -> if List.mem x xs then norepetition xs
             else x::(norepetition xs)

(*Versione alternativa*)
let rec norepetition' = function
    [] -> []
  | x::xs -> x :: (norepetition' (List.filter (fun y -> x<>y) xs))

(*Versione alternativa*)
let rec norepetition'' lst = List.fold_left (fun l x -> if List.mem x l then l else (x::l)) [] lst

(*Calcola i vicini di un nodo senza ripetizioni, considerando il grafo come non orientato*) 
let vicini_norep n g = norepetition (vicini n g) 

(*Calcola i nodi di un grafo: nodes : 'a graph -> 'a list *)
let rec nodes g = norepetition(List.map fst g @ (List.map snd g))

(*Versione alternativa*)
let rec nodes' g = norepetition(List.flatten (List.map (fun (x,y) -> [x;y]) g ))
;;

(*Visita in profondità*) 
let depth_first_collect graph start =
  let rec search visited = function (* search : 'a list -> 'a list -> 'a list *)
      [] -> visited
    | n::rest ->
       if List.mem n visited
       then search visited rest
       else search (n::visited)
              ((successori n graph) @ rest)
              (* oppure: (vicini_norep n graph) @ rest se il grafo non e’ orientato *)
       (* i nuovi nodi sono inseriti in testa *)
  in search [] [start]

(*Visita in ampiezza*)
let breadth_first_collect graph start =
  let rec search visited = function (* search : 'a list -> 'a list -> 'a list *)
      [] -> visited
    | n::rest -> if List.mem n visited (*non si passa per un nodo già visitato*)
                 then search visited rest
                 else search (n::visited) (rest @ (successori n graph)) (* i nuovi nodi sono inseriti in coda *)
                             (* oppure: (rest @ (vicini_norep n graph)) se il grafo non e’ orientato *)
  in search [] [start]
;;

(*Ricerca di un nodo che soddisfi una proprietà e sia raggiungibile da un nodo di ingresso, secondo la visita in profondità: search_node : 'a graph -> 'a -> ('a -> bool) -> 'a *)  
let search_node graph start p =
  let rec search visited = function (* search : 'a list -> 'a list -> 'a *)
      [] -> raise NotExistingNode
    | n::rest -> if List.mem n visited then search visited rest (*non si passa per un nodo già visitato*)
                 else if p n then n
                 else search (n::visited) ((successori n graph) @ rest)
                             (* oppure: ((vicini_norep n graph) @ rest) se il grafo non e’ orientato *)
  in search [] [start]

(*Versione aternativa, secondo la visita in ampiezza*)
let search_node' graph start p =
  let rec search visited = function (* search : 'a list -> 'a list -> 'a *)
      [] -> raise NotExistingNode
    | n::rest -> if List.mem n visited then search visited rest (*non si passa per un nodo già visitato*)
                 else if p n then n
                 else search (n::visited) (rest @ (successori n graph))
                             (* oppure: (rest @ (vicini_norep n graph)) se il grafo non e’ orientato *)
  in search [] [start]

(*Ricerca di un pozzo che sia raggiungibile dal nodo di ingresso: search_sink : 'a graph -> 'a -> 'a *)
let search_sink graph start = search_node graph start (fun x -> successori x graph = [])

(*Determinare se un nodo sia raggiungibile dal nodo di ingresso: reachable : 'a graph -> 'a -> 'a -> bool *)
let reachable graph start n = try  n = (search_node graph start ((=) n)) 
                              with NotExistingNode -> false

(*Versione alternativa*)
let reachable' graph start n = try let _  = (search_node graph start ((=) n)) in true
                               with NotExistingNode -> false

(*Determinare se esiste un ciclo che parta dal nodo di ingresso: exists_cycle : 'a graph -> 'a -> bool *)
let exists_cycle_from_node graph start = List.exists (fun x -> reachable graph x start) (successori start graph)

(*Versione alternativa*)
let exists_cycle_from_node' graph start =
  let rec search visited = function (* search : 'a list -> 'a list -> bool *)
      [] -> false
    | n::ns -> if List.mem n visited then search visited ns (*non si passa per un nodo già visitato*)
               else n=start || search (n::visited) ((successori n graph)@ns)
  in search [] (successori'' start graph)

(*Determinare se esiste un ciclo*)
let exists_cycle graph = List.exists (exists_cycle graph) (nodes graph) 
;;

(*Ricerca di un cammino senza cicli dal nodo di ingresso a un nodo che soddisfi una proprietà: search_path : 'a graph -> 'a -> ('a -> bool) -> 'a list *)
let search_path graph start p =
  let rec from_node visited n = (* from_node : 'a list -> 'a -> 'a list *)
    if List.mem n visited then raise Not_found (*cammino senza cicli non trovato*)
    else if p n then List.rev (n::visited) (*restituisce il cammino trovato*)
    else from_list (n::visited) (successori n graph) (*n è visitato, visita i successori di n*)
  and from_list visited = function (* from_list : 'a list -> 'a list -> 'a list *)
      [] -> raise Not_found 
    | n::rest -> try from_node visited n (*visita un successore*)
                 with Not_found -> from_list visited rest (*bactrack per visitare gli altri successori*)
  in from_node [] start

(*Ricerca di un cammino senza cicli dal nodo di ingresso a un pozzo: search_path_to_sink : 'a graph -> 'a -> 'a list *)
let search_path_to_sink graph start = search_path graph start (fun x -> successori x graph = [])

(*Determinare il valore di un Some (pattern matching parziale, manca il caso None): value : 'a option -> 'a *)
let value (Some x) = x

(*Ricerca di un ciclo che parta dal nodo di ingresso: search_cyle : 'a graph -> 'a -> 'a list *)
let search_cycle graph start = let f = fun x -> try Some (search_path graph x ((=) start))
                                                with Not_found -> None
                               in let paths_some = List.map f (successori start graph) (*paths_some : ('a list option) list *)
                                  in try start :: (value (List.hd(List.filter (fun x -> x<>None) paths_some)))
                                     with _ -> failwith "There is no cycle"

(*Versione alternativa*)
let search_cycle' graph start = let f = fun x -> try Some (search_path graph x ((=) start))
                                                with Not_found -> None
                               in try start :: (List.hd(List.filter_map f (successori start graph)))
                                     with _ -> failwith "There is no cycle"

(*Versione alternativa*)
let search_cycle'' g start =
  let rec aux visited = function (* aux : 'a list -> 'a list -> 'a list *)
      [] -> raise Not_found
    | y::ys -> try if y=start then List.rev (start::visited)
                   else if List.mem y visited then aux visited ys
                   else let s = successori y g in
                        try aux (y::visited) [List.hd s]
                        with _ -> aux (y::visited) (List.tl s)
               with _ -> aux visited ys
  in aux [start] (successori start g)

(*La versione seguente non funziona correttamente. Perché?*)
let search_cycle''' graph start =  start ::  (List.hd(List.map (fun x -> search_path graph x ((=) start)) (successori start graph)))
;;
                                          
