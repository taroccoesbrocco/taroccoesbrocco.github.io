let stampa m =
  let rec ciclo n = 
    if n > m then ()
    else (print_int n;
	  print_newline();
	  ciclo (n+1) )
  in ciclo 0
;;

let somma () =
  let rec loop result =
    try let n = read_int()
        in loop (n+result)
    with _ -> result
  in loop 0
;;

let rec numero_somma () =
  try let n = read_int()
      in let (tot,somma) = numero_somma() 
	 in (tot+1, somma+n)
  with _ -> (0,0)
;;

let rec numero_somma_media () =
  try let n = read_int()
      in let (tot,somma,_) = numero_somma_media () 
	 in (tot+1, somma+n, float_of_int(somma+n)/.float_of_int(tot+1))
  with _ -> (0,0,0.0)
;;

let media () =
    let (tot,somma,media) = numero_somma_media ()
    in (print_int(tot); 
	print_string " ";
	print_int(somma); 
	print_string " ";
	print_float(media);
        print_newline ()
       )
;;
