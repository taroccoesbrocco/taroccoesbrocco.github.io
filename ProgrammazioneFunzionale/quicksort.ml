let rec sort = function
    [] -> []
  | (h::t) -> let (l,r)= List.partition ((<=) h) t
              in (sort l)@h::(sort r)
