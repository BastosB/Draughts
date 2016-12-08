open Game

(* Stupid IA: it takes the first possible valid move. *)
let best_move state =
  match List.filter (is_valid state) (all_moves state) with
  | [] -> assert false
  | m :: _ ->
    let player = turn state in
    (Some m, worst_for player)
