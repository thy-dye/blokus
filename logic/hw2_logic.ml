open! Core

(* Colors and turn Order *)
module Color = struct
  type t =
    | Blue
    | Yellow
    | Red
    | Green
  [@@deriving sexp, compare, equal]

  let all = [ Blue; Yellow; Red; Green ]

  let next t =
    match t with
    | Blue -> Yellow
    | Yellow -> Red
    | Red -> Green
    | Green -> Blue
  ;;
end

(* Board Positions to be usable as Map keys *)
module Coordinate = struct
  module T = struct
    type t =
      { r : int
      ; c : int
      }
    [@@deriving sexp, compare, equal]
  end

  include T
  include Comparable.Make (T) (*allows use in a map*)
end

(* ---------- Piece shapes and transformations ---------- *)
module Piece = struct
  type t = Coordinate.t list [@@deriving sexp, compare, equal]

  (*size of a piece*)
  let size (p : t) = List.length p

  (* Shift so the smallest row and column are 0, then sort, so two equal shapes always
     look identical. *)
  let normalize (p : t) : t =
    let min_r =
      List.fold p ~init:Int.max_value ~f:(fun acc ({ r; _ } : Coordinate.t) ->
        Int.min acc r)
    in
    let min_c =
      List.fold p ~init:Int.max_value ~f:(fun acc ({ c; _ } : Coordinate.t) ->
        Int.min acc c)
    in
    List.map p ~f:(fun ({ r; c } : Coordinate.t) : Coordinate.t ->
      { r = r - min_r; c = c - min_c })
    |> List.sort ~compare:Coordinate.compare
  ;;

  (* 90 degrees clockwise *)
  let rotate_clockwise (p : t) : t =
    normalize
      (List.map p ~f:(fun ({ r; c } : Coordinate.t) : Coordinate.t -> { r = c; c = -r }))
  ;;

  (* 90 degrees counterclockwise*)
  let rotate_counterclockwise (p : t) : t =
    normalize
      (List.map p ~f:(fun ({ r; c } : Coordinate.t) : Coordinate.t -> { r = -c; c = r }))
  ;;

  (* Mirror left-to-right *)
  let reflect (p : t) : t =
    normalize
      (List.map p ~f:(fun ({ r; c } : Coordinate.t) : Coordinate.t -> { r; c = -c }))
  ;;

  let reflect_vertical (p : t) : t =
    normalize
      (List.map p ~f:(fun ({ r; c } : Coordinate.t) : Coordinate.t -> { r = -r; c }))
  ;;

  (* Every distinct way to turn or flip a piece (1 to 8 of them) *)
  let orientations (p : t) : t list =
    let r0 = normalize p in
    let r1 = rotate_clockwise r0 in
    let r2 = rotate_clockwise r1 in
    let r3 = rotate_clockwise r2 in
    let rots = [ r0; r1; r2; r3 ] in
    List.dedup_and_sort (rots @ List.map rots ~f:reflect) ~compare
  ;;

  let of_pairs pairs : t =
    normalize (List.map pairs ~f:(fun (r, c) : Coordinate.t -> { r; c }))
  ;;

  (* All 21 pieces, written as (row, column) pairs *)
  let all : t list =
    List.map
      ~f:of_pairs
      [ (* 1 tile *)
        [ 0, 0 ]
      ; (* 2 tiles *)
        [ 0, 0; 0, 1 ]
      ; (* 3 tiles *)
        [ 0, 0; 0, 1; 0, 2 ]
      ; [ 0, 0; 0, 1; 1, 1 ]
      ; (* 4 tiles *)
        [ 0, 0; 0, 1; 0, 2; 0, 3 ]
      ; [ 0, 0; 0, 1; 0, 2; 1, 2 ]
      ; [ 0, 0; 0, 1; 0, 2; 1, 1 ]
      ; [ 0, 0; 0, 1; 1, 0; 1, 1 ]
      ; [ 0, 0; 1, 0; 1, 1; 2, 1 ]
      ; (* 5 tiles *)
        [ 0, 0; 0, 1; 0, 2; 0, 3; 0, 4 ]
      ; [ 0, 0; 0, 1; 0, 2; 1, 2; 2, 2 ]
      ; [ 0, 0; 0, 1; 0, 2; 0, 3; 1, 1 ]
      ; [ 0, 0; 0, 1; 0, 2; -1, 1; 1, 1 ]
      ; [ 0, 0; 1, 0; 1, 1; 2, 1; 2, 2 ]
      ; [ 0, 0; 0, 1; 0, 2; 0, 3; 1, 3 ]
      ; [ 0, 0; 1, 0; 1, 1; 2, 1; 3, 1 ]
      ; [ 0, 0; 0, 1; 1, 1; 2, 1; 2, 2 ]
      ; [ 0, 0; 0, 1; 1, 1; 2, 1; 1, 2 ]
      ; [ 0, 0; 0, 1; 1, 1; 2, 1; 1, 0 ]
      ; [ 0, 0; 0, 1; 1, 0; 2, 0; 2, 1 ]
      ; [ 0, 0; 0, 1; 0, 2; 1, 2; -1, 2 ]
      ]
  ;;
end

(* Players *)
module Player = struct
  type t =
    { color : Color.t
    ; remaining : Piece.t list
    }
  [@@deriving sexp, compare, equal]

  let create color = { color; remaining = Piece.all }

  (* Computed instead of stored *)
  let tiles_left t = List.sum (module Int) t.remaining ~f:Piece.size
end

(* Game outcome *)
module Decision = struct
  type t =
    | In_progress of { whose_turn : Color.t }
    | Winner of Color.t
    | Stalemate
  [@@deriving sexp, compare, equal]
end

(* A single move *)
module Move = struct
  type t =
    { piece : Piece.t (* the piece as it appears in Piece.all *)
    ; orientation : Piece.t (* after the player's rotations/flips *)
    ; position : Coordinate.t (* where the orientation's (0,0) goes *)
    }
  [@@deriving sexp, compare, equal]

  (* The board squares this move would cover *)
  let squares (t : t) : Coordinate.t list =
    List.map t.orientation ~f:(fun ({ r; c } : Coordinate.t) : Coordinate.t ->
      { r = r + t.position.r; c = c + t.position.c })
  ;;
end

(* The game *)
module Game_state = struct
  type t =
    { board : Color.t Coordinate.Map.t (* missing key = empty square *)
    ; rows : int
    ; columns : int
    ; players : Player.t list
    ; decision : Decision.t
    ; last_move : Move.t option
    }
  [@@deriving sexp, compare, equal]

  module Move_error = struct
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

  let create () : t =
    { board = Coordinate.Map.empty
    ; rows = 20
    ; columns = 20
    ; players = List.map Color.all ~f:Player.create
    ; decision = In_progress { whose_turn = List.hd_exn Color.all }
    ; last_move = None
    }
  ;;

  (* ----- Helpers ----- *)

  let find_player (t : t) (color : Color.t) : Player.t =
    List.find_exn t.players ~f:(fun (p : Player.t) -> Color.equal p.color color)
  ;;

  let in_bounds (t : t) ({ r; c } : Coordinate.t) =
    0 <= r && r < t.rows && 0 <= c && c < t.columns
  ;;

  (* Does this color own this square? Empty or off-board = false *)
  let owns (t : t) (color : Color.t) (pos : Coordinate.t) =
    match Map.find t.board pos with
    | Some owner -> Color.equal owner color
    | None -> false
  ;;

  let has_played (t : t) (color : Color.t) = Map.exists t.board ~f:(Color.equal color)

  let starting_corner (t : t) (color : Color.t) : Coordinate.t =
    match color with
    | Blue -> { r = 0; c = 0 }
    | Yellow -> { r = 0; c = t.columns - 1 }
    | Red -> { r = t.rows - 1; c = t.columns - 1 }
    | Green -> { r = t.rows - 1; c = 0 }
  ;;

  let edge_neighbors ({ r; c } : Coordinate.t) : Coordinate.t list =
    [ { r = r - 1; c }; { r = r + 1; c }; { r; c = c - 1 }; { r; c = c + 1 } ]
  ;;

  let corner_neighbors ({ r; c } : Coordinate.t) : Coordinate.t list =
    [ { r = r - 1; c = c - 1 }
    ; { r = r - 1; c = c + 1 }
    ; { r = r + 1; c = c - 1 }
    ; { r = r + 1; c = c + 1 }
    ]
  ;;

  (* ----- The core functions ----- *)

  let validate_move (t : t) (color : Color.t) (move : Move.t)
    : (unit, Move_error.t) Result.t
    =
    let (player : Player.t) = find_player t color in
    let squares = Move.squares move in
    (* Does any square of the move have a neighbor (by edge or corner) of our color? *)
    let any_square_touches neighbors =
      List.exists squares ~f:(fun square ->
        List.exists (neighbors square) ~f:(owns t color))
    in
    if not (List.mem player.remaining move.piece ~equal:Piece.equal)
    then Error Move_error.Piece_already_used
    else if
      not (List.mem (Piece.orientations move.piece) move.orientation ~equal:Piece.equal)
    then Error Invalid_orientation
    else if not (List.for_all squares ~f:(in_bounds t))
    then Error Out_of_bounds
    else if List.exists squares ~f:(Map.mem t.board)
    then Error Space_already_filled
    else if any_square_touches edge_neighbors
    then Error Touches_own_edge
    else if has_played t color
    then if any_square_touches corner_neighbors then Ok () else Error No_corner_contact
    else if List.mem squares (starting_corner t color) ~equal:Coordinate.equal
    then Ok ()
    else Error Must_cover_starting_corner
  ;;

  (* Can this color place any remaining piece, in any orientation, anywhere? *)
  let has_any_move (t : t) (color : Color.t) : bool =
    let (player : Player.t) = find_player t color in
    List.exists player.remaining ~f:(fun piece ->
      List.exists (Piece.orientations piece) ~f:(fun orientation ->
        List.exists (List.range 0 t.rows) ~f:(fun r ->
          List.exists (List.range 0 t.columns) ~f:(fun c ->
            let (move : Move.t) = { piece; orientation; position = { r; c } } in
            Result.is_ok (validate_move t color move)))))
  ;;

  (* Next player who can still move, or end the game if nobody can *)
  let next_decision (t : t) (current : Color.t) : Decision.t =
    (* Try each color after [current], at most 4 tries (the 4th is [current] again) *)
    let rec find_mover candidate tries_left =
      if tries_left = 0
      then None
      else if has_any_move t candidate
      then Some candidate
      else find_mover (Color.next candidate) (tries_left - 1)
    in
    match find_mover (Color.next current) 4 with
    | Some color -> In_progress { whose_turn = color }
    | None ->
      (* Nobody can move: fewest tiles left wins; a tie is a Stalemate *)
      let scores =
        List.map t.players ~f:(fun (p : Player.t) -> p.color, Player.tiles_left p)
      in
      let best =
        List.map scores ~f:snd |> List.min_elt ~compare:Int.compare |> Option.value_exn
      in
      (match List.filter scores ~f:(fun (_, score) -> score = best) with
       | [ (color, _) ] -> Winner color
       | _ -> Stalemate)
  ;;

  let make_move (t : t) (move : Move.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | Winner _ | Stalemate -> Error Game_is_over
    | In_progress { whose_turn } ->
      (match validate_move t whose_turn move with
       | Error e -> Error e
       | Ok () ->
         (* Fill every square of the piece with the player's color *)
         let board =
           List.fold (Move.squares move) ~init:t.board ~f:(fun board square ->
             Map.set board ~key:square ~data:whose_turn)
         in
         (* Remove the used piece from that player's remaining pieces *)
         let players =
           List.map t.players ~f:(fun (p : Player.t) ->
             if Color.equal p.color whose_turn
             then
               { p with
                 Player.remaining =
                   List.filter p.remaining ~f:(fun piece ->
                     not (Piece.equal piece move.piece))
               }
             else p)
         in
         let t = { t with board; players; last_move = Some move } in
         Ok { t with decision = next_decision t whose_turn })
  ;;
end
