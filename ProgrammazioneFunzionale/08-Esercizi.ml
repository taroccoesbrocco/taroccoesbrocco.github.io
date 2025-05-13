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
;;

let t2 = Tr("A",
           Tr("B",
              Tr("A",Empty,Empty),
              Tr("D",Empty,Empty)
             ),
           Empty
          )
;;

let t0 = Tr (1,
             Tr(2,
                 Empty,
                 Tr (3, Empty, Empty)
               ),
             Tr(4,
                 Empty,
                 Tr (5, Empty, Empty)
               )
           )


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

let rec postorder = function
    Empty -> []
  | Tr(x,left,right) -> (preorder left)@(preorder right)@[x]

let rec preorder = function
    Empty -> []
  | Tr(x,left,right) -> (preorder left)@[x]@(preorder right)
;;
