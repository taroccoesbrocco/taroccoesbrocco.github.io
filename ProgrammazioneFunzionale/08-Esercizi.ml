type expr =
  Int of int
| Var of string
| Sum of expr * expr
| Diff of expr * expr
| Mult of expr * expr
| Div of expr * expr

let rec subexpr e1 e2 = match e1 with
    Int _ | Var _ -> e1 = e2
    | Sum(e',e'') | Diff(e',e'') | Mult(e',e'') | Div(e',e'') ->
       e1=e2 || subexpr e' e2 || subexpr e'' e2

let rec subst_in_expr e x e' = match e with
    Int _ -> e
  | Var y -> if x=y then e' else e
  | Sum(e1,e2) -> Sum (subst_in_expr e1 x e', subst_in_expr e2 x e')
  | Diff(e1,e2) -> Diff (subst_in_expr e1 x e', subst_in_expr e2 x e')
  | Mult(e1,e2) -> Mult (subst_in_expr e1 x e', subst_in_expr e2 x e')
  | Div(e1,e2) -> Div (subst_in_expr e1 x e', subst_in_expr e2 x e')
;;


type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

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

let t1 = Tr("A",
           Tr("B",
              Tr("A",Empty,Empty),
              Tr("D",Empty,Empty)
             ),
           Tr("D",
              Empty,
              Tr("A",Empty,Empty)
             )
          )

let t2 = Tr("A",
           Tr("B",
              Tr("A",Empty,Empty),
              Tr("D",Empty,Empty)
             ),
           Empty
          )

let t0 = Tr(1,
             Tr(2,
                 Empty,
                 Tr (3, Empty, Empty)
               ),
             Tr(4,
                 Empty,
                 Tr (5, Empty, Empty)
               )
           )

let t00 = Tr(0,
             Tr(10,
                Tr(2,Empty,Empty),
                Tr(5,Empty,Empty)),
             Tr(6,
                Tr(6,
                   Tr(3,Empty,Empty),
                   Empty),
                Tr(4,
                   Tr(3,Empty,Empty),
                   Tr(4,Empty,Empty))))

let t000 = Tr(0,
             Tr(10,
                Tr(2,Empty,Empty),
                Tr(5,Empty,Empty)),
             Tr(6,
                Tr(6,
                   Tr(3,Empty,Empty),
                   Tr(1,Empty,Empty)),
                Tr(4,
                   Tr(3,Empty,Empty),
                   Tr(4,Empty,Empty))))

let t000' = Tr(0,
             Tr(10,
                Tr(2,Empty,Empty),
                Tr(5,Empty,Empty)),
             Tr(5,
                Tr(6,
                   Tr(7,Empty,Empty),
                   Tr(1,Empty,Empty)),
                Tr(4,
                   Tr(3,Empty,Empty),
                   Tr(4,Empty,Empty))))
;;

let rec reflect = function
    Empty -> Empty
  | Tr(x,tr1,tr2) -> Tr(x, reflect tr2, reflect tr1)

let fulltree n =
  let rec aux h root = 
    if h=0 then Empty
    else Tr(root, aux (h-1) (2*root), aux (h-1) (2*root+1))
  in aux n 1

let rec balanced = function
    Empty -> true
  | Tr(_,t1,t2) ->
     balanced t1 && balanced t2
     && abs(height t1 - height t2) <= 1

exception NotBalanced
(* balanced : 'a tree -> bool *)
(* balanced t = l’albero t e’ bilanciato *)
(* aux: ’a tree -> int *)
(* aux t solleva NotBalanced se t non e’ bilanciato, altrimenti
riporta l’altezza di t *)
let balanced' t =
  let rec aux = function
      Empty -> -1 (* l’albero vuoto e’ bilanciato e ha altezza -1 *)
    | Tr(_,t1,t2) ->
       let k1 = aux t1 in
       let k2 = aux t2
       (* se uno dei due sottoalberi non e’
          bilanciato, viene sollevata l’eccezione,
          che si propaga *)
       in if abs(k1 - k2) <= 1
          then 1 + max k1 k2
          else raise NotBalanced
  in try let _ = aux t in true
     with NotBalanced -> false

let rec preorder = function
    Empty -> []
  | Tr(x,left,right) -> x::(preorder left)@(preorder right)

(*Versione alternativa*)
let preorder' t =
  let rec aux l = function
      Empty -> l
    | Tr(n,left,right) -> x::(aux (aux l left) right)
  in aux [] t

let rec postorder = function
    Empty -> []
  | Tr(x,left,right) -> (preorder left)@(preorder right)@[x]

let rec inorder = function
    Empty -> []
  | Tr(x,left,right) -> (inorder left)@[x]@(inorder right)
;;

let rec take n = function
    [] -> []
  | x::xs -> if n<=0 then []
             else x::(take (n-1) xs)

let rec drop n = function
    [] -> []
  | l -> if n>0 then drop (n-1) (List.tl l)
         else l

let rec balpreorder = function
    [] -> Empty
  | x::xs -> let n = (List.length xs)/2
             in Tr(x, balpreorder(take n xs), balpreorder(drop n xs))

let rec balinorder = function
    [] -> Empty
  | l -> let n = (List.length l)/2
             in let l' = drop n l
                in Tr(List.hd l', balinorder(take n l), balinorder(List.tl l'))

let balpostorder l = reflect(balpreorder (List.rev l))
;;

let subset l1 l2 = List.for_all (function x -> List.mem x l2) l1

let foglie_in_lista l t =
  let rec aux acc = function
      Empty -> acc
    | Tr(x,Empty,Empty) -> x::acc
    | Tr(_,left,right) -> aux (aux acc left) right
  in subset (aux [] t) l

(*Versione alternativa*)
let lista_foglie t = 
  let rec aux acc = function
      Empty -> acc
    | Tr(x,Empty,Empty) -> x::acc
    | Tr(_,left,right) -> aux (aux acc left) right
  in aux [] t

let foglie_in_lista l t = subset (lista_foglie t) l

let num_foglie t =
  let rec aux acc = function
      Empty -> acc
    | Tr(_,Empty,Empty) -> acc+1
    | Tr(_,l,r) -> aux (aux acc l) r
  in aux 0 t

(*Versione alternativa*)
let num_foglie' t = List.length (lista_foglie t)
;;

exception TooLong

let rec segui_bool l = function
    Empty -> raise TooLong
  | Tr(n,left,right) -> try let b = List.hd l in
                            if b=true then segui_bool (List.tl l) left
                            else segui_bool (List.tl l) right
                        with Failure _ -> n

(*Versione alternativa*)
let rec segui_bool' l t  = match (l,t) with
    (_,Empty) ->  raise TooLong
  | ([],Tr(n,left,right)) ->  n
  | (x::xs,Tr(n,left,right)) -> if x then segui_bool' xs left
                                else segui_bool' xs right
;;


exception MaxNotFound

(*Supponiamo che le foglie siano etichettate da interi nonnegativi*)
let foglia_costi t =
  let rec aux (lst,tot) = function
      Empty -> (lst,tot)
    | Tr(n,Empty,Empty) -> ((n,n+tot)::lst,n+tot)
    | Tr(n,l,r) -> aux (fst(aux (lst,n+tot) l), n+tot)  r
  in fst (aux ([],0) t)

let foglia_costo t =
  let rec aux (a,b) = function
      [] -> (a,b)
    | (n,m)::xs -> if b>m then aux (a,b) xs
                   else aux (n,m) xs
  in match (foglia_costi t) with 
      [] -> raise MaxNotFound
    | (n,m)::xs -> aux (n,m) xs
                     
(*La versione seguente non funziona correttamente. Perché?*)
let foglia_costo' t =
  let rec aux cost tot = function
      Empty -> raise MaxNotFound
    | Tr(n,Empty,Empty) -> let new_tot = cost+n in
                           if new_tot>tot then (n,new_tot)
                           else raise MaxNotFound
    | Tr(n,left,right) -> try aux (cost+n) tot left
                          with MaxNotFound -> aux (cost+n) tot right
  in aux 0 0 t

(*La versione seguente non funziona correttamente. Perché?*)
let foglia_costo'' = function
    Empty -> raise MaxNotFound
  | Tr(n,Empty,Empty) -> (n,n)
  | Tr(n,l,r) ->
     let rec aux cost tot = function
         Empty -> raise MaxNotFound
       | Tr(n,Empty,Empty) -> let new_tot = cost+n in
                              if new_tot>tot then (n,new_tot)
                              else raise MaxNotFound
       | Tr(n,left,right) -> try aux (cost+n) tot left
                             with MaxNotFound -> aux (cost+n) tot right
     in let max_pair (n,x) (m,y) = if x>y then (n,x) else (m,y)
     in max_pair (aux n n l) (aux n n r)
;;


type expr = Jolly
          | Int of int
          | Var of string
          | Sum of expr * expr
          | Diff of expr * expr
          | Mult of expr * expr
          | Div of expr * expr

let rec pattern_matching e m  = match (e,m) with
    (_, Jolly) -> true
  | (Int n, Int m) -> n = m
  | (Var x, Var y) -> x = y
  | (Sum(e1,e2), Sum(m1,m2)) | (Diff(e1,e2), Diff(m1,m2)) | (Mult(e1,e2), Mult(m1,m2)) | (Div(e1,e2), Div(m1,m2)) -> (pattern_matching e1 m1) && (pattern_matching e2 m2)
  | _ -> false
;;


let rec max_common_subtree t1 t2 = match (t1,t2) with
    (Empty,Empty) -> Empty
  | (Empty,Tr(x,l,r)) | (Tr(x,l,r),Empty) -> Tr("@",Empty,Empty)
  | (Tr(x1,l1,r1),Tr(x2,l2,r2)) -> if x1=x2 then Tr(x1,max_common_subtree l1 l2, max_common_subtree r1 r2)
                                   else Tr("@",Empty,Empty)

let rec stessa_struttura t1 t2 = match (t1,t2) with
    (Empty,Empty) -> true
  | (Tr(_,l1,r1),Tr(_,l2,r2)) -> (stessa_struttura l1 l2) && (stessa_struttura r1 r2)
  | _ -> false

let rec is_assoc = function
    [] -> true
  | (x,y)::l -> try y = List.assoc x l  && (is_assoc l)
                with _ -> true

let rec make_mapping t1 t2 =  match (t1,t2) with
      (Empty,Empty) -> []
    | (Tr(n1,l1,r1),Tr(n2,l2,r2)) -> (n1,n2)::(make_mapping l1 l2)@(make_mapping r1 r2)
    | _ -> failwith "No same structure."

let esiste_mapping t1 t2 = try is_assoc (make_mapping t1 t2) 
                           with _ -> false
;;

let path p t =
  let rec aux acc = function
      Empty -> raise Not_found
    | Tr(n,Empty,Empty) -> if not (p n) then n::acc
                           else raise Not_found
    | Tr(n,l,r) -> if not (p n) then
                     try aux (n::acc) l
                     with _ -> aux (n::acc) r
                   else raise Not_found
  in List.rev(aux [] t)
;;

type 'a sostituzione = ('a * 'a tree) list

let rec applica subst = function
    Empty -> Empty
  | Tr(x,Empty,Empty) ->
     (try List.assoc x subst
      with _ -> Tr(x,Empty,Empty) )
  | Tr(x,l,r) -> Tr(x,applica subst l,applica subst r)
;;
