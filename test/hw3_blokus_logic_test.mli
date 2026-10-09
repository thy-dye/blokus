open! Core
open Blokus_logic_library
open Hw2_logic

(* Unwraps [Ok x], raises on [Error] *)
val ok_exn : ('a, 'b) Result.t -> 'a

(* make_move that raises (showing the error) instead of returning [Error] *)
val make_move_exn : Game_state.t -> Move.t -> Game_state.t

(* Prints the top-left [rows] x [columns] of the board (defaults to all of it) *)
val pretty_print_board : ?rows:int -> ?columns:int -> Game_state.t -> unit

(* Every legal move for this color, found by a separate copy of the rules *)
val legal_moves : Game_state.t -> Color.t -> Move.t list

(* Plays a full random game from [seed], checking invariants after every move.
   Returns the final state and how many moves were made. *)
val random_walk : seed:int -> Game_state.t * int