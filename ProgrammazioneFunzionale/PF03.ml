let stampa m =
  let rec ciclo n = 
    if n > m then ()
    else (print_int n;
	  print_newline();
	  ciclo (n+1) )
  in ciclo 0
;;

let rec somma () =
  try let n = read_int ()
      in n + (somma ())
  with _ -> 0

let rec somma' () =
  try let n = read_int ()
      in let result = somma' ()
      in n + result
  with _ -> 0

(*versione iterativa*)
let somma'' () =
  let rec loop result =
    try let n = read_int ()
        in loop (n+result)
    with _ -> result
  in loop 0
;;

let rec numero_somma () =
  try let n = read_int ()
      in let (tot,somma) = numero_somma () 
	 in (tot+1, somma+n)
  with _ -> (0,0)

(*versione iterativa*)
let numero_somma' () =
  let rec aux tot somma =
      try let n = read_int ()
          in aux (tot+1) (somma+n)
      with _ -> (tot,somma)
  in aux 0 0 
;;

let rec numero_somma_media () =
  try let n = read_int()
      in let (tot,somma,_) = numero_somma_media () 
	 in (tot+1, somma+n, float_of_int(somma+n)/.float_of_int(tot+1))
  with _ -> (0,0,0.0)

(*versione iterativa*)
let numero_somma_media' () =
  let rec aux tot somma = 
    try let n = read_int()
        in aux (tot+1) (somma+n)
    with _ -> (tot, somma, float_of_int(somma)/.float_of_int(tot))
  in aux 0 0 
;;

let media () =
    let (tot,somma,media) = numero_somma_media ()
    in (
        print_string "Quantità: ";
        print_int(tot);
        print_newline ();
	print_string "Somma: ";
	print_int(somma);
        print_newline ();
	print_string "Media: ";
	print_float(media);
        print_newline ()
       )
;;

(*destination of the file to read in your device*)
let destination = "./Dropbox/Documenti/Enseignements/ProgrammazioneFunzionale/Programmi/"

let print_one_from_file s = (*prints the first line in the file s*)
  let  ic = open_in (destination^s)
  in try let line = input_line ic
         in print_endline line;
            close_in ic
       with _ -> close_in ic
  
let print_from_file s = (*prints all the lines in the file s*)
  let  ic = open_in (destination^s)
  in let rec loop () =
       try let line = input_line ic
           in print_endline line;
              loop ()
       with _ -> close_in ic
     in loop ()
            
let media_from_file s = (*computes the average of the integers in the file s*)
  let  ic = open_in (destination^s)
  in let rec loop tot somma =
       try let line = input_line ic
           in print_endline line;
              loop (tot+1) (somma+int_of_string(line))
       with _ -> close_in ic;
                 float_of_int(somma)/.float_of_int(tot)
     in loop 0 0 
;;
