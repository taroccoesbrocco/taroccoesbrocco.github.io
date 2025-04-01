let rec merge l1 l2 =
  match (l1,l2) with
    ([],l) | (l,[]) -> l
  | (x::xs,y::ys) -> if x<=y then x::(merge xs (y::ys))
                     else y::(merge (x::xs) ys)
;;

let split l = 
  let rec split_aux l1 l2 = function
      [] -> (l1,l2)
    | [x] -> (x::l1,l2)
    | x::y::xs -> split_aux (x::l1) (y::l2) xs
  in split_aux [] [] l
;;

(*Versione alternativa, più sintetica*)
let split l = 
  let rec split_aux l1 l2 = function
      [] -> (l1,l2)
    | x::xs -> split_aux l2 (x::l1) xs
  in split_aux [] [] l
;;

let rec mergesort = function
    [] -> []
  | [x] -> [x]
  | l -> let (xs,ys) = split l
         in merge (mergesort xs) (mergesort ys)
;;

let rec merge_gen comp l1 l2 =
  match (l1,l2) with
    ([],l) | (l,[]) -> l
    | (x::xs,y::ys) -> if (comp x y)=(-1) then x::(merge_gen comp xs (y::ys))
                       else y::(merge_gen comp (x::xs) ys)
;;

let rec mergesort_gen comp = function
    [] -> []
  | [x] -> [x]
  | l -> let (xs,ys) = split l
         in merge_gen comp (mergesort_gen comp xs) (mergesort_gen comp ys)
;;

let rec downto' m n =
  if m>n then []
  else n :: downto' m (n-1)
;;

let upto m n =
  let rec aux acc m' n' =
    if m'>n' then acc
    else aux (n'::acc) m' (n'-1)
  in aux [] m n
;;

let rec take' n l =
  let rec aux acc n = function
      [] -> acc
    | x::xs -> if n<=0 then acc
               else aux (x::acc) (n-1) xs
  in List.rev (aux [] n l)
;;

let rec flatten = function
    [] -> []
  | x::xs -> x @ (flatten xs)
;;

let flatten' ls =
  let rec aux acc = function
      [] -> acc
    | x::xs -> aux (acc@x) xs
  in aux [] ls
;;

let rec conta n = function
    [] -> 0
  | x::xs -> if n=x then 1 + (conta n xs)
             else conta n xs
;;

let rec conta' acc n = function
    [] -> acc
  | x::xs -> if n=x then conta' (acc+1) n xs
             else conta' acc n xs
;;

let rec contatutti elementi listona =
  match (elementi,listona) with
    ([],l) -> []
  | (x::xs,l) -> (x, conta x listona)::(contatutti xs listona)
;;

let contatutti' elementi listona =
  let rec aux acc elem list =
    match (elem,list) with
      ([],l) -> acc
    | (x::xs,l) -> aux ((x, conta x listona)::acc) xs listona
  in aux [] elementi listona
;;

let comp (_,x) (_,y) = if x<y then -1
                       else if x=y then 0
                       else 1
;;
let sort l = List.sort comp l
;;

let rec primi = function
    [] -> []
  | (x,y)::l -> x::(primi l)
;;

let primi' ls =
  let rec aux acc = function
      [] -> acc
    | (x,y)::l -> aux (x::acc) l
  in List.rev (aux [] ls)
;;

let super estrazioni dim higher =
  primi' (take' dim (sort (contatutti (upto 1 higher) (flatten' estrazioni))))
;;

