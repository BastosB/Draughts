  open Game

(* find_max returns the best playable move

val find_max: player → (move × result) list → move × result *)

let rec find_max player l = 
	match l with 
	|[] -> failwith "Empty list in find_max"
	|(mov,res)::(mov2, res2)::t -> (match (compare player res res2) with 
			|Smaller -> find_max player ((mov,res)::t)
			|Equal -> find_max player ((mov2,res2)::t)
			|Greater -> find_max player ((mov2,res2)::t))
	|(mov,res)::[] -> (mov,res) ;; 

let memory = Hashtbl.create 300000 ;;  
			
let rec cache f = 
    fun arg -> 
      if Hashtbl.mem memory arg then Hashtbl.find memory arg 
      else 
        let nouv = (f arg) in Hashtbl.add memory arg nouv ; Printf.printf "Cache used %d \n%!" (Hashtbl.length memory) ;  
nouv ;;

(*val best_move: state → move option * result*)

exception Found of move option * result

let rec best_move state =
	 	match (result state) with (* result state renvoie le résultat du game*)
	  		|Some x -> (None, x) (* x a gagné, on renvoie None (move) et x (le nom du vainqueur) *)
	  		|None -> if all_moves state = [] then failwith "Empty list in best_move" 
	  				else 
              let (a,c) = find_max (turn state) (List.map (fun x-> (x, (match cache best_move (play state x) with 
                                                                        |(Some m, r) -> r
                                                                        |(None, r) -> r) ) ) (List.filter (is_valid state) (all_moves state)) ) ; 
              in (Some(a),c) ;  
 



(* let rec best_move state =
    match (result state) with (* result state renvoie le résultat du game*)
        |Some x -> (None, x) (* x a gagné, on renvoie None (move) et x (le nom du vainqueur) *)
        |None -> if all_moves state = [] then failwith "Empty list in best_move" 
            else 
              let (a,c) = find_max (turn state) (List.map (fun x-> (x, (match cache best_move (play state x) with 
                                                                        |(Some m, r) -> r
                                                                        |(None, r) -> r) ) ) (List.filter (is_valid state) (all_moves state)) ) ; 

              in (Some(a),c) ;  *)

(* let rec best_move state =

  let rec listeMove liste = match liste with
    | [] -> []
    | move :: suite -> 
        if (is_valid state move) then 
          move :: listeMove suite
        else
          listeMove suite 
  in

  let listeMovevalide= listeMove (all_moves state)
  in
    match result state with
      | Some(resultat) ->  ( None, resultat)
      | None ->
          let rec moveResult liste =
            match liste with
              |[] -> []
              | move :: tail ->

                  let (_,resultNext)= cache best_move ( (play state move) )
                  in
                   if (worst_for (turn state)=resultNext) then
                    (move,resultNext)::(moveResult tail)
                  else 
                    (move,resultNext)::[]
          in
          let listeMoveResult=  moveResult listeMovevalide
          in
          let (move,result)= find_max (turn state) listeMoveResult

          in
            (Some move, result)
;; *) 