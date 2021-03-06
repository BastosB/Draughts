open Gamebase
open Graphics

(*** Abstract representation of a two-player game.
 *
 * The game can be tic-tac-toe, connect 4, chess, whatever. 
 *
*)



(* The type 'state' represents a game configuration.
 *  for example, for tic-tac-toe it is a 3x3 grid 
 *  for connect 4 (puissance 4), it is a 6x7 grid 
 *
 * It contains also the current turn (i.e. which player can play now). *)
type state


(* This type represents a move. 
 *   tic-tac-toe: a coordinate in the grid.
 *   connect 4: a column number. *)
type move


(* This type represents the final result of a game.
 *   For example, it can be the final score of each player (two ints)
 *   or a value of type player indicating the winner (Human wins / Comput wins)
 *   or a three-value result: (Human wins / Comput wins / Even game) *)
type result


(* to_string functions *)
val state2s:   state -> string
val move2s:     move -> string
val result2s: result -> string

(* Reads a move from a string. 
 * Returns None if the string fails to be parsed. *)
val readmove: string -> move option

(* Initial state, when the game starts. *)
val initial3: state
val initial4: state
val initial5: state
val initial6: state
val initial7: state
val initial8: state
val initial9: state
val initial10: state

(* Indicates which player must play now, in the current state. *)
val turn: state -> player

(* Indicates if a move is valid in the current state. *)
val is_valid: state -> move -> bool

(* Play a move. 
 * Changes the current player (unless the current player can play twice). *)
val play: state -> move -> state

(* Returns the list of moves that can be played in the current state, or any superset of it.
 * All_moves will be filtered by is_valid in another part of the program anyway.
 *
 * e.g. in connect 4, it can be a list of all columns (although not all columns are playable). 
 * The argument 'state' can be useful in some games (e.g. chess) *)

val all_moves: state -> move list

val get_all_captures: state -> move list


(* Returns the result of the game. None if the result is not known yet. *)
val result: state -> result option

(* Type used in the result of compare. See below. *)
type comparison = Equal | Greater | Smaller

(* This function will be used by the AI, in order to find the best move.
 *
 *   compare player old_result new_result:
 *
 *   compare player r1 r2  returns Equal if r1 and r2 are equivalent.
 *                         returns Greater if r2 is better than r1 (from the point of view of player). 
 *                         returns Smaller if r1 is better than r2 (from the point of view of player). *)
val compare: player -> result -> result -> comparison

(* This function will also be used by the AI. 
 * 
 * Returns the worst possible result for the given player. Useful for computing min or max.
 * The worst for Human is supposed to be the best for Comput, and conversely. *)
val worst_for: player -> result

val best_for: player -> result

val draw_damier : state -> unit

val readsize : string -> int option

val size_square : int 

val event_list : event list 

val get_move : state -> move

 val draw_move : state -> move -> unit
