open! Core

(* Colors and turn order *)
module Color : sig
  type t =
    | Blue
    | Yellow
    | Red
    | Green
  [@@deriving sexp, compare, equal]

  (* All colors, in turn order *)
  val all : t list

  (* The color whose turn comes after this one *)
  val next : t -> t
end

(* Board positions, usable as Map keys *)
module Coordinate : sig
  type t =
    { r : int
    ; c : int
    }
  [@@deriving sexp]

  (* Provides compare, equal, Map, Set, etc. (what Comparable.Make generates) *)
  include Comparable.S with type t := t
end

(* Piece shapes and transformations *)
module Piece : sig
  type t = Coordinate.t list [@@deriving sexp, compare, equal]

  val size : t -> int
  val normalize : t -> t
  val rotate_clockwise : t -> t
  val rotate_counterclockwise : t -> t
  val reflect : t -> t
  val reflect_vertical : t -> t

  (* Every distinct rotation/reflection of a piece (1 to 8) *)
  val orientations : t -> t list

  val of_pairs : (int * int) list -> t

  (* All 21 pieces *)
  val all : t list
end

(* Players *)
module Player : sig
  type t =
    { color : Color.t
    ; remaining : Piece.t list
    }
  [@@deriving sexp, compare, equal]

  val create : Color.t -> t
  val tiles_left : t -> int
end

(* Game outcome *)
module Decision : sig
  type t =
    | In_progress of { whose_turn : Color.t }
    | Winner of Color.t
    | Stalemate
  [@@deriving sexp, compare, equal]
end

(* A single move *)
module Move : sig
  type t =
    { piece : Piece.t
    ; orientation : Piece.t
    ; position : Coordinate.t
    }
  [@@deriving sexp, compare, equal]

  (* The board squares this move would cover *)
  val squares : t -> Coordinate.t list
end

(* The game *)
module Game_state : sig
  type t =
    { board : Color.t Coordinate.Map.t
    ; rows : int
    ; columns : int
    ; players : Player.t list
    ; decision : Decision.t
    ; last_move : Move.t option
    }
  [@@deriving sexp, compare, equal]

  module Move_error : sig
    type t =
      | Game_is_over
      | Piece_already_used
      | Invalid_orientation
      | Out_of_bounds
      | Space_already_filled
      | Touches_own_edge
      | No_corner_contact
      | Must_cover_starting_corner
    [@@deriving sexp, compare]
  end

  (* A new 20x20 game, Blue to move *)
  val create : unit -> t

  val find_player : t -> Color.t -> Player.t
  val starting_corner : t -> Color.t -> Coordinate.t

  (* Ok () if this color could legally make this move right now *)
  val validate_move : t -> Color.t -> Move.t -> (unit, Move_error.t) Result.t

  (* Does this color have any legal move left? *)
  val has_any_move : t -> Color.t -> bool

  (* Place the current player's piece and advance the turn *)
  val make_move : t -> Move.t -> (t, Move_error.t) Result.t
end