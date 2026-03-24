let ultime_cifre n = (abs((n mod 100)/10), abs(n mod 10))
;;

let bello n =
  let (d,u) = ultime_cifre n
  in match u with
       0 | 3 | 7 -> (match d with
                       0 -> x < 10
                     | 3 | 7 -> false
                     | _ -> true)
       | _ -> false
;;

let data (d,m) = match m with
    "febbraio" -> d>0 && d<29
  | "aprile" | "giugno" | "settembre" | "novembre" -> d>0 && d<31
  | "gennaio" | "marzo" | "maggio" | "luglio"
    | "agosto" | "ottobre" | "dicembre" -> d>0 && d<32
  | _ -> false
;;


let data' (d,m,y) =
  (*bisestile : int -> bool determina se un anno è bisestile o meno*)
  let bisestile n = n mod 400 = 0 || (n mod 100 <> 0 && n mod 4 = 0)
  in match m with
       "febbraio" ->  d>0 && (d<29 || (bisestile y && d<30))
     | "aprile" | "giugno" | "settembre" | "novembre" -> d>0 && d<31
     | "gennaio" | "marzo" | "maggio" | "luglio"
       | "agosto" | "ottobre" | "dicembre" -> d>0 && d<32
     | _ -> false
;;
