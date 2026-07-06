type bool_expr = Var of string
               | Not of bool_expr
               | And of bool_expr * bool_expr
               | Or of bool_expr * bool_expr
;;

let rec eval12 var1 b1 var2 b2 = function
    Var x -> if x = var1 then b1
             else if x = var2 then b2
             else raise Not_found
  | Not expr -> not (eval12 var1 b1 var2 b2 expr)
  | And (expr1, expr2) -> (eval12 var1 b1 var2 b2 expr1) && (eval12 var1 b1 var2 b2 expr2)
  | Or (expr1, expr2) -> (eval12 var1 b1 var2 b2 expr1) || (eval12 var1 b1 var2 b2 expr2)
;;

let table2 var1 var2 expr =
  [(true, true, eval12 var1 true var2 true expr);
   (true, false, eval12 var1 true var2 false expr);
   (false, true, eval12 var1 false var2 true expr);
   (false, false, eval12 var1 false var2 false expr)]

(*Versione alternativa*)
let table2' var1 var2 expr =
  List.map (fun (b1,b2) -> (b1, b2, eval12 var1 b1 var2 b2 expr)) [(true,true); (true,false); (false,true); (false,false)]
;;

let rec eval val_vars = function
    Var x -> List.assoc x val_vars
  | Not expr -> not (eval val_vars expr)
  | And (expr1, expr2) -> (eval val_vars expr1) && (eval val_vars expr2)
  | Or (expr1, expr2) -> (eval val_vars expr1) || (eval val_vars expr2)
;;

let rec table_maker dict vars expr = match vars with
    [] -> [(dict, eval dict expr)]
  | x::xs -> table_maker ((x,true)::dict) xs expr @ (table_maker ((x,false)::dict) xs expr)

let table vars expr = table_maker [] vars expr

(*Versione alternativa*)
let table' vars expr =
  let rec combine = function
      [] -> [[]]
    | x::xs -> let dicts = combine xs
               in List.map (fun d -> ((x,true)::d)) dicts @
                    List.map (fun d -> ((x,false)::d)) dicts
  in List.map (fun a -> (a, eval a expr)) (combine vars)
;;
