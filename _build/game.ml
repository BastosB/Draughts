open Gamebase

(* These types are abstract in game.mli *)

(* For the moment, we just implement the "count-20" game. 
 * Keep these types as such. *)

type state = ((char matrix) * player)  

type move = ((int * int) * (int * int)) (* Move from A(x,y) to B(x',y') *)

type result = Win of player | Even  

(* Printers.
 * Guess what sprintf does (not  printf). *)
let state2s (m,p) = Printf.sprintf "Current = \n\n%s\n\n  --> %s to play\n%!" (matrix2s m Char.escaped) (player2s p)

let move2s ((x,y),(u,v)) = Printf.sprintf "Move from (%d,%d) to (%d,%d)\n%!" x y u v 

let result2s = function
	|Even -> "Even game !!"
	|Win(p) -> (player2s p) ^ " WINS !! "

(* Reader 
 * We use sscanf here. (In this simple case, int_of_string would suffice.) *)
let readmove s =
  try Some (Scanf.sscanf s "%d %d %d %d" (fun x y u v -> ((x,y),(u,v))))
  with _ -> None

(* You have to provide these. *)
let initial = ([|[| ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
		  [| 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' |] ; 
		  [| ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
		  [| 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' |] ; 
		  [| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		  [| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		  [| ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' |] ; 
		  [| 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' |] ; 
		  [| ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' |] ; 
		  [| 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' |] |], Human)
		  

let turn (m,p) = p 

let in_board mat (x,y) = 
	if ( (0 <= x) && (x < Array.length mat) && (0 <= y) && (y < Array.length mat) ) then true 
	else false 

(*val is_valid: state -> move -> bool*)
let is_valid (mat,pla) ((x,y),(u,v)) = 
	if not((in_board mat (x,y)) && (in_board mat (u,v))) then false (* If the coordonates are not if the board, false*)
	else 
		(match pla with 
		| Comput -> (if (mat.(x)).(y) = 'b' then 
						(if (u = (x+1)) && ( (v=(y-1)) || (v=(y+1)) ) then true else false) 
					else false)
(*A FAIRE : else if (*le pion mange un pion : attention, un pion peut manger en arrière *) *)
		| Human -> (if (mat.(x)).(y) = 'w' then 
						(if (u = (x-1)) && ( (v=(y-1)) || (v=(y+1))) then true else false)
					else false))
(*A FAIRE : else if (*le pion mange un pion : attention, un pion peut manger en arrière *) *) 



(* val play: state -> move -> state *)
let play (mat,pla) ((x,y),(u,v)) = 
	let new_mat = clone_matrix mat in
	match pla with 
	| Human -> (new_mat.(x)).(y) <- ' ' ; (new_mat.(u)).(v) <- 'w' ; (new_mat,Comput)
	| Comput -> (new_mat.(x)).(y) <- ' ' ; (new_mat.(u)).(v) <-'b' ; (new_mat,Human)	
	
(*val all_moves: state -> move list*)
let all_moves (mat, pla) = (* déplacement simple : pas de prise en compte des pions adverses *) 
let move_list = ref [] in 
	for x = 0 to Array.length mat - 1 do
      let line = mat.(x) in
      for y = 0 to Array.length line - 1 do
		match pla with 
		|Comput -> if (mat.(x)).(y) = 'b' then move_list := ((x,y),((x+1),(y-1))) :: ((x,y),((x+1),(y+1))) :: !move_list else move_list := !move_list (* on est les noirs, on va vers le bas *)
		|Human ->  if (mat.(x)).(y) = 'w' then move_list :=((x,y),((x-1),(y-1))) :: ((x,y),((x-1),(y+1))) :: !move_list else move_list := !move_list
      done ;
    done;
!move_list 


(*val result: state -> result option*)
let result (mat,pla) = 
	match pla with 
	| Human -> if List.filter (is_valid (mat,pla)) (all_moves (mat,pla)) = [] then Some(Win(Comput)) else None 
	| Comput -> if List.filter (is_valid (mat,pla)) (all_moves (mat,pla)) = [] then Some(Win(Human)) else None

(* AUTRE MANIERE DE VOIR SI LE JEU EST FINI : (ne marche pas, je sais pas pourquoi*)
	(*match pla with 
	|Human -> (match find_cell mat (fun c ->  c = 'b') with
			|None -> Some(Win(Human))
			|Some x -> None)
	|Comput -> (match find_cell mat (fun c -> c = 'w')  with
			|None -> Some (Win(Comput))
			|Some x -> None)*)
(* This type was given in game.mli.
 * We have to repeat it here. *)
type comparison = Equal | Greater | Smaller

let compare player r1 r2 = 
	let other_player = match player with 
				|Human -> Comput 
				|Comput -> Human in 
	
	(if r1 = r2 then Equal 
	else if ((r2 = Win(player)) || (r2 = Even && r1 = Win(other_player))) then Greater
	else Smaller)
	
(* val worst_for: player -> result *)
let worst_for player = 
	match player with 
	|Human -> Win(Comput)
  	|Comput -> Win(Human)
  
