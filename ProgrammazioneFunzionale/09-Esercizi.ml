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

let rec preorder (Tr(n,ts)) = n::(List.flatten (List.map preorder ts))

let rec postorder (Tr(n,ts)) = (List.flatten (List.map postorder ts))@[n]

let rec inorder (Tr(n,ts)) =
  try (inorder (List.hd ts))@[n]@(List.flatten (List.map inorder (List.tl ts)))
  with _ ->  [n]
;;

let rec foglie_in_lista l = function
    Tr(n,[]) -> List.mem n l
  | Tr(_,ts) -> List.for_all (foglie_in_lista l) ts

let num_di_foglie t =
  let rec aux tot = function
      Tr(_,[]) -> tot+1
    | Tr(_,ts) -> List.fold_right (+) (List.map (aux tot) ts) 0
  in aux 0 t

let rec listaGuida l t = match (l,t) with
    ([],Tr(n,ts)) -> n
  | (n::xs,Tr(_,ts)) -> listaGuida xs (List.nth ts n)

let tutte_foglie_costi t =
  let rec aux tot l = function
      Tr(n,[]) -> (n,tot+n)::l
    | Tr(n,ts) -> List.flatten (List.map (aux (tot+n) l) ts) 
  in aux 0 [] t

let foglia_costo t =
  let rec aux (n,cost) = function
      [] -> (n,cost)
    | (n',cost')::xs -> if cost'>cost then aux (n',cost') xs
                        else aux (n,cost) xs
  in let l = tutte_foglie_costi t
     in aux (List.hd l) (List.tl l) 
;;
