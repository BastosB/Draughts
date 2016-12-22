open Gamebase
open Game
open Functory.Network
open Functory.Network.Same

(* Interactively ask for the player's move. 
 * Returns Some move, or None when the move is invalid. *)
let ask_move state =
  Printf.printf "  => Your move ? %!" ;  
  let line = read_line () in

  match readmove line with
  | None ->
    Printf.printf "\n Cannot read this move: %s\n\n%!" line ;
    None
    
  | Some mov ->
    if not (is_valid state mov) then
      begin
        Printf.printf "\n This move is invalid: %s\n\n%!" (move2s mov) ;
        None
      end
    else Some mov

(* Get the move from the IA. *)
let ia_move state =
  let (mov, _) = Game_ia.divide state in
  match mov with
  | None -> assert false
  | Some m -> Printf.printf "%s" (move2s m) ; m 
  
(*** Each player in turn. ***)

  let () = Functory.Control.set_debug true

  
  let rec run with_ia state =


  (* Print state & which player to play. *)
  Printf.printf "\n%s\n %s to play.\n\n%!" (state2s state) (player2s (turn state)) ;
  Printf.printf "Valid moves : \n " ;
  if (all_moves state) = [] then Printf.printf "Aucun mouvement possible ! \n%!" 
  else  List.iter (Printf.printf " %s\n%! ")  (List.map move2s (List.filter (is_valid state) (all_moves state)))  ;
  
  Printf.printf "----------------- \n " ;


  
  match result state with
  | Some r ->
    (* Game is finished. Print result. *)
    Printf.printf "*** %s ***\n%!" (result2s r) ;
    ()
    
  | None ->
    (* Game is not finished. Play one turn. *)

    let state' =
      if with_ia && turn state = Comput
      then play state (ia_move state)
      else
        begin match ask_move state with
          | None -> state (* Invalid move, play same state again. *)
          | Some mov -> play state mov
        end
    in
    run with_ia state'


(*let () = run true initial*)

let () =
  
  (* Sys.argv are the command-line arguments. *)
  match Sys.argv with

  (* If there is one argument equal to "master" *)
  | [| _ ; "master" |] -> 
     Printf.printf "I am the master.\n%!" ;
     (* set_number_of_cores 4 ; *)
         declare_workers ~n:2 "localhost" ;
         declare_workers ~n:2 "192.168.1.92" ;
     run true initial

  (* Otherwise, we are a worker. *)
  | _ -> 
     Printf.printf "I am a worker.\n%!" ;
     Functory.Network.Same.Worker.compute ()