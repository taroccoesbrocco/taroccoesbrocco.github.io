let rec inits = function
    [] -> []
  | x::xs -> [x] :: (List.map ((@) [x]) (inits xs))
;;

(*Versione alternativa*)
let rec inits' = function
    [] -> []
  | x::xs -> [x] :: (List.map (List.cons x) (inits xs))
;;

let from_morse = function
    ".-" -> 'a'
  | "-..." -> 'b'
  | "-.-." -> 'c'
  | "-.." -> 'd'
  | "." -> 'e'
  | "..-." -> 'f'
  | "--." -> 'g'
  | "...." -> 'h'
  | ".." -> 'i'
  | ".---" -> 'j'
  | "-.-" -> 'k'
  | ".-.." -> 'l'
  | "--" -> 'm'
  | "-.." -> 'n'
  | "---" -> 'o'
  | ".--." -> 'p'
  | "--.-" -> 'q'
  | ".-." -> 'r'
  | "..." -> 's'
  | "-" -> 't'
  | "..-" -> 'u'
  | "...-" -> 'v'
  | "-..-" -> 'x'
  | "-.--" -> 'y'
  | "--.." -> 'z'
  | ".----" -> '1'
  | "..---" -> '2'
  | "...--" -> '3'
  | "....-" -> '4'
  | "....." -> '5'
  | "-...." -> '6'
  | "--..." -> '7'
  | "---.." -> '8'
  | "----." -> '9'
  | "-----" -> '0'
  | _ -> failwith "Not in the Morse alphabet"
;;

let decode_morse l = List.map from_morse l
;;

let to_morse = function
    'a' -> ".-"
  | 'b' -> "-..."
  | 'c' -> "-.-."
  | 'd' -> "-.."
  | 'e' -> "."
  | 'f' -> "..-."
  | 'g' -> "--."
  | 'h' -> "...."
  | 'i' -> ".."
  | 'j' -> ".---"
  | 'k' -> "-.-"
  | 'l' -> ".-.."
  | 'm' -> "--"
  | 'n' -> "-.."
  | 'o' -> "---"
  | 'p' -> ".--."
  | 'q' -> "--.-"
  | 'r' -> ".-."
  | 's' -> "..."
  | 't' -> "-"
  | 'u' -> "..-"
  | 'v' -> "...-"
  | 'x' -> "-..-"
  | 'y' -> "-.--"
  | 'z' -> "--.."
  | '1' -> ".----"
  | '2' -> "..---"
  | '3' -> "...--"
  | '4' -> "....-"
  | '5' -> "....."
  | '6' -> "-...."
  | '7' -> "--..."
  | '8' -> "---.."
  | '9' -> "----."
  | '0' -> "-----"
  | _ -> failwith "Not in the Morse alphabet"
;;

let encode_morse l = List.map to_morse l
;;

let rec powerset = function
    [] -> [[]]
  | x::xs -> let pxs = powerset xs
             in pxs @ (List.map (List.coms x) pxs)
;;

(*La versione seguente non funziona correttamente. Perché?*)
let rec powerset' = function
    [] -> []
  | x::xs -> let pxs = powerset xs
             in pxs @ (List.map (List.coms x) pxs)
;;

(*Versione iterativa*)
let powerset'' l =
  let rec aux acc = function
      [] -> acc
    | x::xs -> aux (acc @ (List.map (List.cons x) acc)) xs
  in aux [[]] l
;;

(*La versione seguente non funziona correttamente. Perché?*)
let powerset''' l =
  let rec aux acc = function
      [] -> acc
    | x::xs -> aux (acc @ (List.map (List.cons x) acc)) xs
  in aux [[]] l
;;

let rec cartprod l = function
    [] -> []
  | y::ys -> (List.map (function x -> (x,y)) l) @ (cartprod l ys)
;;

(*Versione iterativa*)
let cartprod' l1 l2 =
  let rec aux acc l = function
      [] -> acc
    | y::ys -> aux (acc @ (List.map (function x -> (x,y)) l)) l ys
  in aux [] l1 l2
;;

