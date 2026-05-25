type multi_expr = MultiInt of int
                | MultiVar of string
                | MultiDiff of multi_expr * multi_expr
                | MultiDiv of multi_expr * multi_expr
                | MultiSum of multi_expr list
                | MultiMult of multi_expr list

let e = MultiSum [MultiInt 5; MultiVar "x"] 

let rec subexpr e e' = match e with
    MultiInt _ | MultiVar _ | MultiSum [] |  MultiMult [] -> e = e'
    | MultiDiff(e1,e2) | MultiDiv(e1,e2) -> e=e' || subexpr e1 e' || subexpr e2 e'
    | MultiSum(x::xs) | MultiMult(x::xs) -> e=e' || subexpr x e' || List.exists (function x -> subexpr x e') xs

let rec subst e var e' = match e with
    MultiInt _ -> e
  | MultiVar x -> if x=var then e' else e
  | MultiDiff(e1,e2) -> MultiDiff(subst e1 var e', subst e2 var e')
  | MultiDiv(e1,e2) -> MultiDiv(subst e1 var e', subst e2 var e')
  | MultiSum l -> MultiSum (List.map (function x -> subst x var e') l)
  | MultiMult l -> MultiMult (List.map (function x -> subst x var e') l)
;;


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

let t' = Tr(1,
           [Tr(2,[])])

let rec preorder (Tr(n,ts)) = n::(List.flatten (List.map preorder ts))

let rec postorder (Tr(n,ts)) = (List.flatten (List.map postorder ts))@[n]

let rec inorder (Tr(n,ts)) =
  try (inorder (List.hd ts))@[n]@(List.flatten (List.map inorder (List.tl ts)))
  with _ ->  [n]

(*Versione alternativa*)
let rec inorder' (Tr(n,ts)) = match ts with
    [] -> [n]
  | x::xs -> (inorder' x)@[n]@(List.flatten (List.map inorder' xs))

(*Versione alternativa*)
let rec inorder'' (Tr(n,ts)) =
  let start = try inorder'' (List.hd ts)
              with _ -> [] 
  and finish = try List.flatten (List.map inorder'' (List.tl ts))
               with _ -> []
  in start@[n]@finish
;;

let rec foglie_in_lista l = function
    Tr(n,[]) -> List.mem n l
  | Tr(_,ts) -> List.for_all (foglie_in_lista l) ts

let rec num_di_foglie = function
    Tr(_,[]) -> 1
  | Tr(_,ts) -> List.fold_left (+) 0 (List.map num_di_foglie ts)

(*Versione alternativa*)
let rec num_di_foglie' = function
    Tr(_,[]) -> 1
  | Tr(_,ts) -> List.fold_right (+) (List.map num_di_foglie' ts) 0

(*Versione alternativa*)
let rec num_di_foglie'' (Tr(_,ts)) =
  match List.fold_left (+) 0 (List.map num_di_foglie'' ts) with
    0 -> 1
  | n -> n

(*Versione alternativa*)
let num_di_foglie''' t =
  let rec aux tot = function (* aux : int -> 'a ntree -> int *)
      Tr(_,[]) -> tot+1
    | Tr(_,ts) -> List.fold_left (+) 0 (List.map (aux tot) ts)
  in aux 0 t

(*Versione alternativa*)
let num_di_foglie'''' t =
  let rec aux tot = function (* aux : int -> 'a ntree -> int *)
      Tr(_,[]) -> tot+1
    | Tr(_,ts) -> List.fold_right (+) (List.map (aux tot) ts) 0
  in aux 0 t

(*La versione seguente non funziona correttamente. Perché?*)
let rec num_di_foglie_wrong (Tr(_,ts)) = List.fold_left (+) 0 (List.map num_di_foglie_wrong ts)

(*La versione seguente non funziona correttamente. Perché?*)
let rec num_di_foglie_wrong' (Tr(_,ts)) = List.fold_left (+) 1 (List.map num_di_foglie_wrong' ts)
;;

let rec listaGuida l t = match (l,t) with
    ([],Tr(n,ts)) -> n
  | (n::xs,Tr(_,ts)) -> listaGuida xs (List.nth ts n)
;;

let tutte_foglie_costi t =
  let rec aux tot l = function (* aux : int -> int ntree list -> (int*int) list *)
      Tr(n,[]) -> (n,tot+n)::l
    | Tr(n,ts) -> List.flatten (List.map (aux (tot+n) l) ts) 
  in aux 0 [] t

let foglia_costo t =
  let rec aux (n,cost) = function (* aux : int*int -> (int*int) list -> int*int *)
      [] -> (n,cost)
    | (n',cost')::xs -> if cost'>cost then aux (n',cost') xs
                        else aux (n,cost) xs
  in let l = tutte_foglie_costi t
     in aux (List.hd l) (List.tl l)

(*Versione alternativa*)
let foglia_costo' t = List.hd (List.sort (fun (_,costo1) (_,costo2) -> compare costo2 costo1) (tutte_foglie_costi t))

(*Versione alternativa*)
let foglia_costo'' t = let l = tutte_foglie_costi t
                       in List.fold_left (fun (foglia1,costo1) (foglia2,costo2) -> if costo1>costo2 then (foglia1,costo1)
                                                                                   else (foglia2,costo2))
                            (List.hd l) (List.tl l)

(*Versione alternativa*)
let foglia_costo t = (*backtrack*)
  let rec aux (n,cost) tot = function (* aux : int*int -> int -> int ntree list -> int*int *)

let ramo_da_lista t l k =
  let rec aux acc = function 
      Tr(n,[]) -> if n=k then n::acc
                 else raise Not_found
    | Tr(n,ts) -> try if List.mem n l then
                        try List.hd (List. map (aux (n::acc)) ts)
                        with Failure _ -> raise Not_found
                      else raise Not_found
                  with _ -> raise Not_found
  in aux [] t
;;

let rec same_structure t1 t2 = match (t1,t2) with
    (Tr(_,ts1),Tr(_,ts2)) -> try List.for_all2 same_structure ts1 ts2
                             with _ -> false
;;
