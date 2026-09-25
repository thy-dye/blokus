open! Core

(*************************************************)
(*              Declarations of Types            *)
(*************************************************)

type color =
  | Red
  | Blue
  | Green
  | Yellow

type coordinate =
  { r : int
  ; c : int
  }

(* Defining what a Piece is *)
type piece = coordinate list

type pieces =
  { one_tile : piece list
  ; two_tile : piece list
  ; three_tile : piece list
  ; four_tile : piece list
  ; five_tile : piece list
  }
(* will be used for the question isValidMoveAvail? *)

(* Defining the Game State and Players *)
type player =
  { color : color
  ; remaining_pieces : pieces
  ; tiles : int
  }

type decision =
  | In_progress of { whose_turn : player }
  | Winner of player
  | Stalemate

(* figure out later if i want a new gamestate each time or mutate it *)
type game_state =
  { rows : int
  ; columns : int
  ; mutable board : color option array array
  ; decision : decision
  ; players : player list
    (* add players to the game state *)
    (* everything should be in the game state *)
  }
(*************************************************)
(*           Initialization of variables         *)
(*************************************************)

(* hardcoded pieces since they are constant per game *)
let all_pieces = {
 one_tile = [ 
  [{r=0; c=0}]; 
 ]; 
 two_tile = [
  [{r=0; c=0}; {r=0; c=1}];
 ];
 three_tile = [
  [{r=0; c=0}; {r=0; c=1}; {r=0; c=2}];
  [{r=0; c=0}; {r=0; c=1}; {r=1; c=1}];
 ];
 four_tile = [
  [{r=0; c=0}; {r=0; c=1}; {r=0; c=2}; {r=0; c=3};];
  [{r=0; c=0}; {r=0; c=1}; {r=0; c=2}; {r=1; c=2};];
  [{r=0; c=0}; {r=0; c=1}; {r=0; c=2}; {r=1; c=1};];
  [{r=0; c=0}; {r=0; c=1}; {r=1; c=1}; {r=1; c=0};];
  [{r=0; c=0}; {r=1; c=0}; {r=1; c=1}; {r=2; c=1};];
 ];
 five_tile = [
  [{r=0; c=0}; {r=0; c=1}; {r=0; c=2}; {r=0; c=3}; {r=0; c=4};];
  [{r=0; c=0}; {r=0; c=1}; {r=0; c=2}; {r=1; c=2}; {r=2; c=2};];
  [{r=0; c=0}; {r=0; c=1}; {r=0; c=2}; {r=0; c=3}; {r=1; c=1};];
  [{r=0; c=0}; {r=0; c=1}; {r=0; c=2}; {r=(-1); c=1}; {r=1; c=1};];
  [{r=0; c=0}; {r=1; c=0}; {r=1; c=1}; {r=2; c=1}; {r=2; c=2};];
  [{r=0; c=0}; {r=0; c=1}; {r=0; c=2}; {r=0; c=3}; {r=1; c=3};];
  [{r=0; c=0}; {r=1; c=0}; {r=1; c=1}; {r=2; c=1}; {r=3; c=1};];
  [{r=0; c=0}; {r=0; c=1}; {r=1; c=1}; {r=2; c=1}; {r=2; c=2};];
  [{r=0; c=0}; {r=0; c=1}; {r=1; c=1}; {r=2; c=1}; {r=1; c=2};];
  [{r=0; c=0}; {r=0; c=1}; {r=1; c=1}; {r=2; c=1}; {r=1; c=0};];
  [{r=0; c=0}; {r=0; c=1}; {r=1; c=0}; {r=2; c=0}; {r=2; c=1};];
  [{r=0; c=0}; {r=0; c=1}; {r=0; c=2}; {r=1; c=2}; {r=(-1); c=2};];
 ];
}

let red_player : player =
{
  color = Red;
  remaining_pieces = all_pieces; (*remaining_pieces*)
  tiles = 89;
}

let blue_player : player =
{
  color = Blue;
  remaining_pieces = all_pieces;
  tiles = 89;
}

let green_player : player =
{
  color = Green;
  remaining_pieces = all_pieces;
  tiles = 89;
}

let yellow_player : player =
{
  color = Yellow;
  remaining_pieces = all_pieces;
  tiles = 89;
}

let initial : game_state =
{
  rows = 20;
  columns = 20;
  board = Array.make_matrix ~dimx:20 ~dimy:20 None;
  decision = In_progress { whose_turn = red_player };
  players = [red_player; blue_player; green_player; yellow_player]
}
