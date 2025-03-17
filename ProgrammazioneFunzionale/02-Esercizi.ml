let ultime_cifre n = (abs((n mod 100)/10), n mod 10)
;;

let bello n =
  let (d,u) = ultime_cifre n
  in match u with
       0 | 3 | 7 -> (match d with
                      3 | 7 -> false
                      | _ -> true)
       | _ -> false
;;

let data (d,m) =
  match m with
    "febbraio" -> d>0 && d<29
  | "aprile" | "giugno" | "settembre" | "novembre" -> d>0 && d<31
  | "gennaio" | "marzo" | "maggio" | "luglio"
    | "agosto" | "ottobre" | "dicembre" -> d>0 && d<32
  | _ -> false
;;

  
