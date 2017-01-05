  open Game
  open Functory.Network
  open Functory.Network.Same
  open Unix


(* find_max returns the best playable move

val find_max: player → (move × result) list → move × result *)

let rec find_max player l = 
	match l with 
	|[] -> failwith "Empty list in find_max"
	|(mov,res)::(mov2, res2)::t -> 
      (match (compare player res res2) with 
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

let rec best_move state =
    match (result state) with (* result state renvoie le résultat du game*)
        |Some x -> (None, x) (* x a gagné, on renvoie None (move) et x (le nom du vainqueur) *)
        |None ->  if all_moves state = [] then failwith "Empty list in best_move" 
                  else 
                    let (a,c) = find_max (turn state) (List.map (fun x-> (x, (match cache best_move (play state x) with 
                                                                        |(Some m, r) -> r
                                                                        |(None, r) -> r) ) ) (List.filter (is_valid state) (all_moves state)) ) 

                    in (Some(a),c) ;;  

  (* C'est ma fonction fold *)
  (* val find_max_acu : player -> move option * result -> move option * result -> move option * result *)
  let rec find_max_acu player (acu_m, acu_r) (map_m, map_r) = 
      match (compare player acu_r map_r) with 
      |Smaller -> (acu_m,acu_r)
      |Equal ->  (map_m,map_r)
      |Greater -> (map_m,map_r) ;;

  (* C'est ma fonction map *)
  let compute_all state move =
    match (result state) with (* result state renvoie le résultat du game*)
        |Some x ->  assert false (* On ne doit jamais arriver ici, car cette fonction est toujours appelée après une vérification de l'état.*)
        |None -> match best_move (play state move) with 
                  | (Some x, res) -> (Some move, res)
                  | (None, res) -> (Some move, res) ;; 


let divide state = 
  let moves_list = (List.filter (is_valid state) (all_moves state)) in 
    map_fold_ac ~f:(compute_all state) ~fold:(find_max_acu (turn state)) (None,(worst_for (turn state))) moves_list ;; 