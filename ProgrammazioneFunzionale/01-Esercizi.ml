let pi = 3.14159;;
let area x = pi *. x;;
let pi = 0.0;;
let x = "pippo";;
let a = area 3.0 ;;

let y = 100 ;;
let x = 5 ;;
let h x = y+x ;;
let y = 0 ;;
h 7 ;;

let punct x = x = '.' || x = ',' or x = ';' ;;
punct ;;

let quadrupla =
  (5,('c',"antonio",(),if 3>4 then 0 else 1),"pippo",true) ;;
let pi1 (x,y,z,w) = x ;;
let pi2 (x,y,z,w) = y ;;
let pi3 (x,y,z,w) = z ;;
let pi4 (x,y,z,w) = w ;;
pi3 (pi2 quadrupla) ;;
pi4 (pi2 quadrupla) ;;

if E then true else false ;;
E ;;

if E then false else true ;;
not E ;;

if E then F else false ;;
E && F ;;

if E then F else true ;;
(not E) || F ;;

if E then true else F ;;
E || F ;;

if E then false else F ;;
(not E) && F ;;
