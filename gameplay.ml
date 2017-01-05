open Gamebase
open Game
open Functory.Network
open Functory.Network.Same
open Graphics

(* Interactively ask for the player's move. 
 * Returns Some move, or None when the move is invalid. *)
let ask_move state valid_moves_list =
  Printf.printf "  => Your move ? \n%!" ;  

  let mov = get_move state in 
    if not (List.exists (fun x-> mov=x) valid_moves_list) then (Printf.printf "\n This move is invalid: %s\n\n%!" (move2s mov) ; None)  
    else (Printf.printf "Move played : %s\n%!" (move2s mov) ; Some mov)

    
(* Get the move from the IA. *)
let ia_move state =
  let debut = Unix.gettimeofday() in 
  let (mov, _) = Game_ia.divide state in
  let fin = Unix.gettimeofday() in 
  let duree = fin -. debut in
  Printf.printf "\n\n\n Time of reseach = %f seconds\n\n" duree ; 
  match mov with
  | None -> assert false
  | Some m -> Printf.printf "%s" (move2s m) ; m 
  

(*** Each player in turn. ***)
let () = Functory.Control.set_debug true
 
  let rec run with_ia state =

  (* Print state & which player to play. *)
  Printf.printf "\n%s\n\n%!" (state2s state) ;
  Printf.printf "Valid moves : \n " ;
  if (all_moves state) = [] then Printf.printf "Aucun mouvement possible ! \n%!" 
  else  let all_valid_moves = (List.filter (is_valid state) (all_moves state)) in 
  List.iter (Printf.printf " %s\n%! ")  (List.map move2s all_valid_moves)  ;
  
  Printf.printf "--------------------------------- \n " ;


  
  match result state with
  | Some r ->
    (* Game is finished. Print result. *)
    Printf.printf "*** %s ***\n%!" (result2s r) ;
    ()
    
  | None ->
    (* Game is not finished. Play one turn. *)

    let state' =
      if with_ia && turn state = Comput
      then 
        (let calculated_move = (ia_move state) in 
        draw_move state calculated_move ; 
        play state calculated_move)
      else
        begin match ask_move state all_valid_moves with
          | None -> state (* Invalid move, play same state again. *)
          | Some mov -> draw_move state mov ; play state mov
        end
    in
    run with_ia state'


let () =
  
  (* Sys.argv are the command-line arguments. *)
  match Sys.argv with
  (* If there is one argument equal to "master" *)
  | [| _ ; "master" |] -> 
      Printf.printf "I am the master.\n%!" ;
      Printf.printf "  => Your size ? %!" ;  
    
  let line = read_line () in
  
  let size = match readsize line with
  | Some x -> x
  | None -> (-1) in 
  
  if size < 3 || size > 10 then failwith "Incorrect size" ; 

  let initial = 
    match size with 
    | 3 -> initial3
    | 4 -> initial4
    | 5 -> initial5
    | 6 -> initial6
    | 7 -> initial7
    | 8 -> initial8
    | 9 -> initial9
    | 10 -> initial10
    | _ -> assert false in

  open_graph "" ;
  set_window_title "Are you a Draughts master ?" ;
  resize_window (size * size_square) (size * size_square) ; 
  draw_damier initial ;
  declare_workers ~n:4 "localhost" ;
  (*declare_workers ~n:2 "192.168.1.92" ;*)
  run true initial ; 
  Printf.printf "You can now visualize the result of the game. Click to exit\n%!" ; 
  loop_at_exit [Button_down ; Button_up] (fun status -> raise Exit) ;

  (* Otherwise, we are a worker. *)
  | _ -> 
     Printf.printf "I am a worker.\n%!" ;
     Functory.Network.Same.Worker.compute ()