fold (fun a b -> a ^ " " ^ string_of_int b) "" [ 1 ; 2 ; 3 ; 4]
fold (fun a b -> if a < b then b else a) 0 [ 10 ; 40 ; 20 ; 30 ]
fold (fun a b -> b :: a) [] [ 4 ; 3 ; 2 ; 1 ]