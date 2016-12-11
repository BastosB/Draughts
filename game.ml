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
(*let initial = ([|
		[| ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
		[| 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' |] ; 
		[| ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
		[| 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' |] ; 
		[| ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' |] |], Human) *)
(* let initial = ([|
		[| ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
		[| 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' |] ; 
		[| ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' |] ; 
		[| ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' |] |], Human) 	*)


(* let initial = ([|
		[| ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
		[| 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' |] |], Human) 	*)

	let initial = ( [| 	
						[| 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
						[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
						[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
						[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
						[| 'w' ; ' ' ; 'w' ; ' ' ; 'w' |]|], Human)

(* let initial = ([|
		[| ' ' ; 'b' ; ' ' ; 'b' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' |] |], Human) 	*)




		(* let initial = ( [| 	
						[| 'b' ; ' ' ; 'b' |] ; 
						[| ' ' ; ' ' ; ' ' |] ; 
						[| 'w' ; ' ' ; 'w' |]|], Human)	  *)


let turn (m,p) = p 

let nextPlayer state = 
	match (turn state) with 
        | Human -> Comput 
        | Comput -> Human 

let in_board mat (x,y) = 
	if ( (0 <= x) && (x < Array.length mat) && (0 <= y) && (y < Array.length mat.(x)) ) then true 
	else false 

(*val is_valid: state -> move -> bool*)
let is_valid (mat,pla) ((x,y),(u,v)) = 
	if not((in_board mat (x,y)) && (in_board mat (u,v))) then false (* If the coordonates are not if the board, false*)
	else 
		(match pla with 
		| Comput -> (if (mat.(x)).(y) = 'b' then 
						(if (u = (x+1)) && ( (v = (y-1)) || (v=(y+1)) ) && mat.(u).(v) = ' ' then true (* Basic move *)
						else if (u = (x+2)) || (u = (x-2)) then true else false) (* This is a capture. By construction, it is valid. *)
					else false)
		| Human -> (if (mat.(x)).(y) = 'w' then 
						(if (u = (x-1)) && ( (v=(y-1)) || (v=(y+1))) && mat.(u).(v) = ' ' then true 
						else if (u = (x+2)) || (u = (x-2)) then true 
						else false) (* This is a capture. By construction, it is valid. *)
					else false))



(* val play: state -> move -> state *)
let play (mat,pla) ((x,y),(u,v)) = 
	let next_player = match pla with 
								| Human -> Comput
								| Comput -> Human in 
	
	if (u=x+1) || (u=x-1) then (
		let new_mat = clone_matrix mat in (
			(new_mat.(x)).(y) <- ' ' ; 
			(new_mat.(u)).(v) <- (match pla with 
									| Human -> 'w'
									| Comput -> 'b') ;
			(new_mat,next_player)))
	else if (u=x+2) || (u=x-2) then (
		let new_mat = clone_matrix mat in (
			(new_mat.(x)).(y) <- ' ' ; 
			(new_mat.(u)).(v) <- (match pla with 
									| Human -> 'w'
									| Comput -> 'b') ; 
			(new_mat.(x-(x-u)/2)).(y-(y-v)/2) <- ' ' ;
			(new_mat,next_player)))
else assert false 


let get_all_captures (mat,pla) = 
	let capture_list = ref [] in 
	for x = 0 to Array.length mat - 1 do
      let line = mat.(x) in
      for y = 0 to Array.length line - 1 do
		match pla with 
		|Comput -> (if x+1 < Array.length mat -1 && y+1 < Array.length mat.(x) -1 && (mat.(x)).(y) = 'b' && (mat.(x+1)).(y+1) = 'w' && (mat.(x+2)).(y+2) = ' ' then capture_list := ((x,y),((x+2),(y+2))) :: !capture_list else capture_list := !capture_list ;
					if x+1< Array.length mat -1 && y-1 > 0 && (mat.(x)).(y) = 'b' && (mat.(x+1)).(y-1) = 'w' && (mat.(x+2)).(y-2) = ' ' then capture_list := ((x,y),((x+2),(y-2))) :: !capture_list else capture_list := !capture_list ;
					if x-1 > 0 && y+1 < Array.length mat.(x) -1 && (mat.(x)).(y) = 'b' && (mat.(x-1)).(y+1) = 'w' && (mat.(x-2)).(y+2) = ' ' then capture_list := ((x,y),((x-2),(y+2))) :: !capture_list else capture_list := !capture_list ;
					if x-1 > 0 && y-1 > 0 && (mat.(x)).(y) = 'b' && (mat.(x-1)).(y-1) = 'w' && (mat.(x-2)).(y-2) = ' ' then capture_list := ((x,y),((x-2),(y-2))) :: !capture_list else capture_list := !capture_list )



		|Human ->  (if x+1 < Array.length mat -1 && y+1 < Array.length mat.(x) -1 && (mat.(x)).(y) = 'w' && (mat.(x+1)).(y+1) = 'b' && (mat.(x+2)).(y+2) = ' ' then capture_list := ((x,y),((x+2),(y+2))) :: !capture_list else capture_list := !capture_list ;
					if x+1 < Array.length mat -1 && y-1 > 0 && (mat.(x)).(y) = 'w' && (mat.(x+1)).(y-1) = 'b' && (mat.(x+2)).(y-2) = ' ' then capture_list := ((x,y),((x+2),(y-2))) :: !capture_list else capture_list := !capture_list ;
					if x-1 > 0 && y+1 < Array.length mat.(x) -1 && (mat.(x)).(y) = 'w' && (mat.(x-1)).(y+1) = 'b' && (mat.(x-2)).(y+2) = ' ' then capture_list := ((x,y),((x-2),(y+2))) :: !capture_list else capture_list := !capture_list ;
					if x-1 > 0 && y-1 > 0 && (mat.(x)).(y) = 'w' && (mat.(x-1)).(y-1) = 'b' && (mat.(x-2)).(y-2) = ' ' then capture_list := ((x,y),((x-2),(y-2))) :: !capture_list else capture_list := !capture_list )
      done ;
    done;
!capture_list



	
(*val all_captures: state -> move list*)
let all_moves (mat, pla) =  
	let all_captures = get_all_captures (mat,pla) in 
		if all_captures = [] then
		(let move_list = ref [] in (* déplacement simple *)
			for x = 0 to Array.length mat - 1 do
		      let line = mat.(x) in
		      for y = 0 to Array.length line - 1 do
				match pla with 
				|Comput -> if (mat.(x)).(y) = 'b' then move_list := ((x,y),((x+1),(y-1))) :: ((x,y),((x+1),(y+1))) :: !move_list else move_list := !move_list
				|Human ->  if (mat.(x)).(y) = 'w' then move_list :=((x,y),((x-1),(y-1))) :: ((x,y),((x-1),(y+1))) :: !move_list else move_list := !move_list
		      done ;
		    done; 
		!move_list)
		else all_captures (* It is possible, and so a men MUST capture another one *)


	(* Il faut que j'arrive à trouver tous les enchainements qui bouffent les pions*)



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
  
