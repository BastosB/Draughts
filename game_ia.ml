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
	|(mov,res)::[] -> (mov,res) 
			
			
			
(*val best_move: state → move option * result*)

let rec best_move state =
 	match (result state) with (* result state renvoie le résultat du game*)
  		|Some x -> (None, x) (* x a gagné, on renvoie None (move) et x (le nom du vainqueur) *)
  		|None -> if all_moves state = [] then failwith "Empty list in best_move" else 
  		(let l = List.filter (is_valid state) (all_moves state) in
				let (a,c) = find_max (turn state) (List.map (fun x-> (x,
				(match best_move (play state x) with 
				|(Some m, r) -> r  
  				|(None, r) -> r))) l)
  			in (Some(a),c))
  			
  			
  			
  			  				
