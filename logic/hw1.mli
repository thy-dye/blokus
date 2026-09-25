open! Core

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

type game_state =
  { rows : int
  ; columns : int
  ; mutable board : color option array array
  ; decision : decision
  ; players : player list
  }

val all_pieces : pieces
val initial : game_state