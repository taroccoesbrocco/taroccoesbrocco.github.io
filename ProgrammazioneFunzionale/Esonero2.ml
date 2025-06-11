type 'a ntree = Tr of 'a * 'a ntree list

let t = Tr('a', [Tr('f', [Tr('g', [])]); Tr('c', []);
          Tr('b', [Tr('d', []); Tr('e', [])])])


let rec from_stringlist_to_string = function
    [] -> ""
  | [x] -> x
  | x::xs -> x^" "^(from_stringlist_to_string xs)

(*Versione alternativa*)
let from_stringlist_to_string l = match l with
    [] -> ""
  | _ -> let rec aux acc = function
             [] -> acc
           | x::xs -> aux (acc^" "^x) xs
         in aux (List.hd l) (List.tl l)

(*Versione alternativa*)
let from_stringlist_to_string l =
  let rec aux acc = function
      [] -> acc
    | [x] -> acc^x
    | x::xs -> aux (acc^x^" ") xs
  in aux "" l

(*Versione alternativa*)
let from_stringlist_to_string = function
    [] -> ""
  | x::xs -> List.fold_left (^) "" (x::(List.map (fun x -> " "^x) xs))


let rec lispy = function
    Tr(n,[]) -> String.make 1 n
  | Tr(n,ts) -> "("^(String.make 1 n)^" "^from_stringlist_to_string(List.map lispy ts)^")"

(*Versione alternativa*)
let rec lispy = function
    Tr(n,[]) -> String.make 1 n
  | Tr(n,ts) -> "("^from_stringlist_to_string((String.make 1 n)::(List.map lispy ts))^")"
;;



type 'a graph = ('a * 'a) list

let graph = [(1,2); (1,3); (1,4); (2,6); (3,5); (4,6);
             (5,4); (6,5); (6,7)]


let degree g n = List.length (List.filter (function (n1,n2) -> n=n1 || n=n2) g)

(*Versione alternativa*)
let degree g n = List.fold_right (function (x,y) -> function  acc -> if x=n || y=n then acc+1 else acc) g 0

(*Versione alternativa*)
let degree g n = List.fold_left (+) 0 (List.map (function (x,y) -> if x=n || y=n then 1 else 0) g)
                     
(*Versione alternativa*)
let degree g n =
  let rec aux acc = function
      [] -> acc
    | (n1,n2)::es -> if n=n1 || n=n2 then aux (acc+1) es
                     else aux acc es
  in aux 0 g


let nodes g =
  let rec aux acc = function
      [] -> acc
    | (n,m)::es -> let n_mem = List.mem n acc
                   in let m_mem = List.mem m acc
                      in if n_mem && m_mem then aux acc es
                         else if n_mem then aux (m::acc) es
                         else if m_mem then aux (n::acc) es
                         else aux (n::m::acc) es
  in aux [] g

(*Versione alternativa*)
let nodes g =
  let add x l = if (not (List.mem x l)) then x::l else l
  in let rec aux acc = function
         [] -> acc
       | (a,b)::xs -> aux (add a (add b acc)) xs
     in aux [] g


let nodes_with_degree g = let nodes_g = nodes g
                          in List.combine nodes_g (List.map (degree g) (nodes_g))

(*Versione alternativa*)
let nodes_with_degree g = List.map (function x -> (x, degree g x)) (nodes g)


let ordered_nodes g = List.map fst (List.sort (function (_,d1) -> function (_,d2) -> -(compare d1 d2)) (nodes_with_degree g))
;;





