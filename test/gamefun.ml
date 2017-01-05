open Graphics
open Gamebase
open Game

let size_square = 70 ;; 

let event_list = [Button_down ; Button_up] ;;

let initial = ( [| 	
		[| 'b' ; ' ' ; 'b' ; ' ' ; 'b' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| ' ' ; ' ' ; ' ' ; ' ' ; ' ' |] ; 
		[| 'w' ; ' ' ; 'w' ; ' ' ; 'w' |]|], Human) ;;




let draw_damier (mat, pla) = (* size in [1..10]*)

	for i = 0 to Array.length mat -1 do 
		let line = mat.(i) in
		for j = 0 to Array.length line -1 do 
			if (i+j) mod 2 = 0 then fill_rect (i * size_square) (j * size_square) size_square size_square ; 
			if mat.(i).(j) = 'w' then fill_circle (size_square/2 + i * size_square) (size_square/2 + j * size_square) (size_square*3/4)
		done ; 
	done ;;

	
let readsize s =
  try Some (Scanf.sscanf s "%d" (fun x -> x))
  with _ -> None ;; 


let () = 
	 Printf.printf "  => Your size ? %!" ;  
  	let line = read_line () in
	
	let size = match readsize line with
	| Some x -> x
	| None -> (-1) in 
	
	if size < 1 then failwith "Incorrect size" ; 
	

	open_graph "" ;
	set_window_title "Are you a Draughts master ?" ;
	resize_window (size * size_square) (size * size_square) ; 
	draw_damier initial ; 
	let status = wait_next_event event_list in 
		Printf.printf "Current status = %s\n%!" (string_of_bool status.button) ; 

	loop_at_exit event_list (fun status -> ()) ;; (*Printf.printf "Exit in %d, %d" (status.mouse_x) (status.mouse_y) ) *)
	(*close_graph () ; *)

