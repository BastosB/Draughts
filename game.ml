open Gamebase
open Functory.Network
open Functory.Network.Same
open Functory.Cores
open Sys
open Graphics

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
let initial10 = ([|
		[| ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
		[| 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' |] ; 
		[| ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
		[| 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' |] ; 
		[| ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' |] |], Human)

let initial9 = ([|
		[| ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' |] ; 
		[| 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
		[| ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' |] ; 
		[| ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' |] |], Human)

let initial8 = ([|
		[| ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
		[| 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' |] ; 
		[| ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' |] ; 
		[| ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' |] |], Human) 

let initial7 = ([|
		[| ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' |] ; 
		[| 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' |] |], Human) 


let initial6 = ([|
		[| ' ' ; 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
		[| 'b' ; ' ' ; 'b' ; ' ' ; 'b' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; 'w' ; ' ' ; 'w' ; ' ' ; 'w' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' ; 'w' ; ' ' |] |], Human) 

let initial5 = ( [| 	
		[| 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' ; 'w' |]|], Human)

let initial4 = ([|
		[| ' ' ; 'b' ; ' ' ; 'b' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' |] |], Human) 


let initial3 = ( [| 	
		[| 'b' ; ' ' ; 'b' |] ; 
		[| ' ' ; ' ' ; ' ' |] ; 
		[| 'w' ; ' ' ; 'w' |]|], Human)	


let turn (m,p) = p 


let in_board mat (x,y) = 
	if ( (0 <= x) && (x < Array.length mat) && (0 <= y) && (y < Array.length mat.(x)) ) then true 
	else false 

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

(*val is_valid: state -> move -> bool*)
let is_valid (mat,pla) ((x,y),(u,v)) = 
	if not((in_board mat (x,y)) && (in_board mat (u,v))) then false (* If the coordonates are not if the board, false*)
	else 
		(let all_captures = get_all_captures (mat,pla) in 
		if all_captures = [] then
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
		else 
			(if List.mem ((x,y),(u,v)) all_captures then true 
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


(*val result: state -> result option*)
let result (mat,pla) = 
	match pla with 
	| Human -> if List.filter (is_valid (mat,pla)) (all_moves (mat,pla)) = [] then Some(Win(Comput)) else None 
	| Comput -> if List.filter (is_valid (mat,pla)) (all_moves (mat,pla)) = [] then Some(Win(Human)) else None


(* This type was given in game.mli.
 * We have to repeat it here. *)
type comparison = Equal | Greater | Smaller

let compare player r1 r2 = 
	if r1 = r2 then Equal 
	else if (r2 = Win(player)) || (r2 = Even && r1 = Win(next player)) then Greater
	else Smaller

(* val worst_for: player -> result *)
let worst_for player = 
	match player with 
	|Human -> Win(Comput)
  	|Comput -> Win(Human)
 
 let best_for player = 
	match player with 
	|Human -> Win(Human)
  	|Comput -> Win(Comput)



let size_square = 70 ;; 

let event_list = [Button_down ; Button_up] ;;

let human_color = red ;;

let comput_color = green ;;


let draw_damier (mat,pla) = 

	for i = 0 to Array.length mat - 1 do
      let line = mat.(i) in
      for j = 0 to Array.length line - 1 do
			if (i+j) mod 2 = 0 then (set_color black ; fill_rect (j * size_square) (i * size_square) size_square size_square ; )
      done ;
    done ;

	for i = 0 to Array.length mat - 1 do
      let line = mat.(i) in
      for j = 0 to Array.length line - 1 do
			if mat.(i).(j) = 'w' then (set_color human_color ; fill_circle (size_square/2 + j * size_square) (size_square/2 + (Array.length mat -1 - i) * size_square) (size_square/4) ) ; 
			if mat.(i).(j) = 'b' then (set_color comput_color ; fill_circle (size_square/2 + j * size_square) (size_square/2 + (Array.length mat -1 - i) * size_square) (size_square/4) ) ; 
      done ;
    done ;;

	
let readsize s =
  try Some (Scanf.sscanf s "%d" (fun x -> x))
  with _ -> None ;;


  let find_coordinates_x mat x = 
  	let rec loop acu = 
  		if (acu * size_square) < x && x < ((acu + 1) * size_square) then acu
  		else loop (acu+1)
  	in loop 0 ;; 



  let find_coordinates_y mat y = 
    let rec loop acu = 
  		if ((Array.length mat -1 - acu) * size_square) < y && y < ((Array.length mat -1 - (acu - 1)) * size_square) then acu
  		else loop (acu+1)
  	in loop 0 ;; 


  let get_move (mat,pla) = 
	let status1 = wait_next_event [Button_down] in 
	let status2 = wait_next_event [Button_up] in 
	let from_x = find_coordinates_x mat status1.mouse_x in 
	let from_y = find_coordinates_y mat status1.mouse_y in 
	let to_x = find_coordinates_x mat status2.mouse_x in 
	let to_y = find_coordinates_y mat status2.mouse_y in 

	((from_y,from_x), (to_y,to_x))


	let draw_move (mat,pla) ((x,y),(u,v)) = 
		if (u=x+1) || (u=x-1) then 
			(set_color black ; 
			fill_rect (y*size_square)  ((Array.length mat -1 - x) * size_square) size_square size_square ; 
			match pla with 
				|Human -> set_color human_color ; fill_circle (size_square/2 + v * size_square) (size_square/2 + (Array.length mat -1 - u) * size_square) (size_square/4) ;
				|Comput -> set_color comput_color ; fill_circle (size_square/2 + v * size_square) (size_square/2 + (Array.length mat -1 - u) * size_square) (size_square/4) ;)
		else if (u=x+2) || (u=x-2) then 
			(set_color black ; 
			fill_rect ((y-(y-v)/2)*size_square)  ((Array.length mat -1 - (x-(x-u)/2)) * size_square) size_square size_square ; 
			fill_rect (y*size_square)  ((Array.length mat -1 - x) * size_square) size_square size_square ; 
			match pla with 
				|Human -> set_color human_color ; fill_circle (size_square/2 + v * size_square) (size_square/2 + (Array.length mat -1 - u) * size_square) (size_square/4) ;
				|Comput -> set_color comput_color ; fill_circle (size_square/2 + v * size_square) (size_square/2 + (Array.length mat -1 - u) * size_square) (size_square/4) ;)

	else assert false