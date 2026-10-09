open! Core
open Blokus_logic_library
open Hw2_logic

(* ===================================================================== *)
(* Helpers                                                                *)
(* ===================================================================== *)

let pos r c : Coordinate.t = { r; c }

let check cond msg = if not cond then raise_s [%message "Check failed" (msg : string)]

let ok_exn result =
  match result with
  | Ok x -> x
  | Error _ -> failwith "ok_exn: got Error"
;;

let make_move_exn (t : Game_state.t) (move : Move.t) : Game_state.t =
  match Game_state.make_move t move with
  | Ok t -> t
  | Error e ->
    raise_s [%message "make_move failed" (e : Game_state.Move_error.t) (move : Move.t)]
;;

(* Build a move. Without ~orientation, the piece is placed unrotated. *)
let mv ?orientation piece ~r ~c : Move.t =
  { piece; orientation = Option.value orientation ~default:piece; position = pos r c }
;;

let mono = List.nth_exn Piece.all 0
let domino = List.nth_exn Piece.all 1
let l_tetromino = List.nth_exn Piece.all 5

let print_decision (t : Game_state.t) = print_s [%sexp (t.decision : Decision.t)]

(* Prints (Ok <decision after the move>) or (Error <reason>) *)
let print_move_result (result : (Game_state.t, Game_state.Move_error.t) Result.t) =
  let summary = Result.map result ~f:(fun (t : Game_state.t) -> t.decision) in
  print_s [%sexp (summary : (Decision.t, Game_state.Move_error.t) Result.t)]
;;

let print_validate result =
  print_s [%sexp (result : (unit, Game_state.Move_error.t) Result.t)]
;;

let color_letter (c : Color.t) =
  match c with
  | Color.Blue -> 'B'
  | Color.Yellow -> 'Y'
  | Color.Red -> 'R'
  | Color.Green -> 'G'
;;

(* Prints the top-left [rows] x [columns] of the board ('.' = empty) *)
let pretty_print_board ?rows ?columns (t : Game_state.t) =
  let rows = Option.value rows ~default:t.rows in
  let columns = Option.value columns ~default:t.columns in
  for r = 0 to rows - 1 do
    print_endline
      (String.init columns ~f:(fun c ->
         match Map.find t.board (pos r c) with
         | None -> '.'
         | Some color -> color_letter color))
  done
;;

let print_piece (p : Piece.t) =
  let max_r = List.fold p ~init:0 ~f:(fun acc (sq : Coordinate.t) -> Int.max acc sq.r) in
  let max_c = List.fold p ~init:0 ~f:(fun acc (sq : Coordinate.t) -> Int.max acc sq.c) in
  for r = 0 to max_r do
    print_endline
      (String.init (max_c + 1) ~f:(fun c ->
         if List.mem p (pos r c) ~equal:Coordinate.equal then '#' else '.'))
  done
;;

(* Replace one color's remaining pieces (used to set up end-game situations) *)
let with_remaining (t : Game_state.t) (color : Color.t) pieces : Game_state.t =
  { t with
    Game_state.players =
      List.map t.players ~f:(fun (p : Player.t) ->
        if Color.equal p.color color then { p with Player.remaining = pieces } else p)
  }
;;

let with_board (t : Game_state.t) squares : Game_state.t =
  { t with Game_state.board = Coordinate.Map.of_alist_exn squares }
;;

(* ===================================================================== *)
(* Pieces                                                                 *)
(* ===================================================================== *)

let%expect_test "the piece set is complete" =
  let count n = List.count Piece.all ~f:(fun p -> Piece.size p = n) in
  printf
    "pieces: %d, sizes 1-5: %d %d %d %d %d, total tiles: %d\n"
    (List.length Piece.all)
    (count 1)
    (count 2)
    (count 3)
    (count 4)
    (count 5)
    (List.sum (module Int) Piece.all ~f:Piece.size);
  (* No two pieces are the same shape in disguise *)
  let canonical p =
    List.min_elt (Piece.orientations p) ~compare:Piece.compare |> Option.value_exn
  in
  printf
    "distinct shapes: %d\n"
    (List.length (List.dedup_and_sort (List.map Piece.all ~f:canonical) ~compare:Piece.compare));
  [%expect
    {|
    pieces: 21, sizes 1-5: 1 1 2 5 12, total tiles: 89
    distinct shapes: 21
    |}]
;;

let%expect_test "number of distinct orientations per piece" =
  let counts = List.map Piece.all ~f:(fun p -> List.length (Piece.orientations p)) in
  print_endline (String.concat ~sep:" " (List.map counts ~f:Int.to_string));
  printf "total: %d\n" (List.sum (module Int) counts ~f:Fn.id);
  [%expect
    {|
    1 2 2 4 2 8 4 1 4 2 4 8 1 4 8 8 4 8 8 4 4
    total: 91
    |}]
;;

let%expect_test "rotating and reflecting the L tetromino" =
  print_piece l_tetromino;
  print_endline "clockwise:";
  print_piece (Piece.rotate_clockwise l_tetromino);
  print_endline "counterclockwise:";
  print_piece (Piece.rotate_counterclockwise l_tetromino);
  print_endline "reflect:";
  print_piece (Piece.reflect l_tetromino);
  print_endline "reflect_vertical:";
  print_piece (Piece.reflect_vertical l_tetromino);
  [%expect
    {|
    ###
    ..#
    clockwise:
    .#
    .#
    ##
    counterclockwise:
    ##
    #.
    #.
    reflect:
    ###
    #..
    reflect_vertical:
    ..#
    ###
    |}]
;;

let%expect_test "pieces written with negative coordinates are normalized" =
  print_piece (List.nth_exn Piece.all 12);
  print_endline "--";
  print_piece (List.nth_exn Piece.all 20);
  [%expect
    {|
    .#.
    ###
    .#.
    --
    ..#
    ###
    ..#
    |}]
;;

let%expect_test "transformation laws hold for every piece" =
  let law name f = printf "%s: %b\n" name (List.for_all Piece.all ~f) in
  let cw = Piece.rotate_clockwise in
  law "four clockwise turns = identity" (fun p -> Piece.equal (cw (cw (cw (cw p)))) p);
  law "clockwise then counterclockwise = identity" (fun p ->
    Piece.equal (Piece.rotate_counterclockwise (cw p)) p);
  law "counterclockwise = three clockwise" (fun p ->
    Piece.equal (Piece.rotate_counterclockwise p) (cw (cw (cw p))));
  law "reflect twice = identity" (fun p -> Piece.equal (Piece.reflect (Piece.reflect p)) p);
  law "reflect_vertical = reflect then 180" (fun p ->
    Piece.equal (Piece.reflect_vertical p) (cw (cw (Piece.reflect p))));
  law "transformations keep size" (fun p ->
    List.for_all (Piece.orientations p) ~f:(fun o -> Piece.size o = Piece.size p));
  law "every orientation is normalized" (fun p ->
    List.for_all (Piece.orientations p) ~f:(fun o -> Piece.equal (Piece.normalize o) o));
  law "orientations are closed under rotate/reflect" (fun p ->
    let os = Piece.orientations p in
    List.for_all os ~f:(fun o ->
      List.mem os (cw o) ~equal:Piece.equal
      && List.mem os (Piece.reflect o) ~equal:Piece.equal));
  [%expect
    {|
    four clockwise turns = identity: true
    clockwise then counterclockwise = identity: true
    counterclockwise = three clockwise: true
    reflect twice = identity: true
    reflect_vertical = reflect then 180: true
    transformations keep size: true
    every orientation is normalized: true
    orientations are closed under rotate/reflect: true
    |}]
;;

let%expect_test "Move.squares shifts the orientation to the position" =
  let small_l = List.nth_exn Piece.all 3 in
  Move.squares (mv small_l ~r:5 ~c:3)
  |> List.map ~f:(fun (sq : Coordinate.t) -> sprintf "(%d,%d)" sq.r sq.c)
  |> String.concat ~sep:" "
  |> print_endline;
  [%expect {| (5,3) (5,4) (6,4) |}]
;;

(* ===================================================================== *)
(* Game creation                                                          *)
(* ===================================================================== *)

let%expect_test "a new game" =
  let t = Game_state.create () in
  print_decision t;
  printf "board squares: %d\n" (Map.length t.board);
  List.iter t.players ~f:(fun (p : Player.t) ->
    printf "%c has %d tiles\n" (color_letter p.color) (Player.tiles_left p));
  List.iter Color.all ~f:(fun color ->
    let corner = Game_state.starting_corner t color in
    printf "%c starts at (%d,%d)\n" (color_letter color) corner.r corner.c);
  printf "Blue has a move: %b\n" (Game_state.has_any_move t Blue);
  [%expect
    {|
    (In_progress (whose_turn Blue))
    board squares: 0
    B has 89 tiles
    Y has 89 tiles
    R has 89 tiles
    G has 89 tiles
    B starts at (0,0)
    Y starts at (0,19)
    R starts at (19,19)
    G starts at (19,0)
    Blue has a move: true
    |}]
;;

(* ===================================================================== *)
(* Move validation                                                        *)
(* ===================================================================== *)

let%expect_test "first move must cover the starting corner" =
  let t = Game_state.create () in
  print_move_result (Game_state.make_move t (mv mono ~r:5 ~c:5));
  print_move_result (Game_state.make_move t (mv mono ~r:0 ~c:0));
  [%expect
    {|
    (Error Must_cover_starting_corner)
    (Ok (In_progress (whose_turn Yellow)))
    |}]
;;

let%expect_test "orientation decides which squares are covered" =
  let t = Game_state.create () in
  (* clockwise L leaves (0,0) empty; counterclockwise L covers it *)
  print_move_result
    (Game_state.make_move
       t
       (mv l_tetromino ~orientation:(Piece.rotate_clockwise l_tetromino) ~r:0 ~c:0));
  let t =
    make_move_exn
      t
      (mv l_tetromino ~orientation:(Piece.rotate_counterclockwise l_tetromino) ~r:0 ~c:0)
  in
  pretty_print_board t ~rows:4 ~columns:4;
  [%expect
    {|
    (Error Must_cover_starting_corner)
    BB..
    B...
    B...
    ....
    |}]
;;

let%expect_test "invalid orientations are rejected" =
  let t = Game_state.create () in
  (* claiming to play the 1-tile piece but placing a 2-tile shape *)
  print_move_result (Game_state.make_move t (mv mono ~orientation:domino ~r:0 ~c:0));
  (* a real shape, but not normalized *)
  let small_l = List.nth_exn Piece.all 3 in
  print_move_result
    (Game_state.make_move
       t
       (mv small_l ~orientation:[ pos 1 1; pos 1 2; pos 2 2 ] ~r:0 ~c:0));
  [%expect
    {|
    (Error Invalid_orientation)
    (Error Invalid_orientation)
    |}]
;;

let%expect_test "pieces must stay on the board" =
  let t = Game_state.create () in
  print_move_result (Game_state.make_move t (mv domino ~r:0 ~c:19));
  print_move_result (Game_state.make_move t (mv mono ~r:(-1) ~c:0));
  print_move_result (Game_state.make_move t (mv mono ~r:20 ~c:0));
  [%expect
    {|
    (Error Out_of_bounds)
    (Error Out_of_bounds)
    (Error Out_of_bounds)
    |}]
;;

(* Everyone plays the 1-tile piece in their corner *)
let four_corners () =
  let t = Game_state.create () in
  let t = make_move_exn t (mv mono ~r:0 ~c:0) in
  let t = make_move_exn t (mv mono ~r:0 ~c:19) in
  let t = make_move_exn t (mv mono ~r:19 ~c:19) in
  make_move_exn t (mv mono ~r:19 ~c:0)
;;

let%expect_test "turn order is Blue, Yellow, Red, Green" =
  let t = Game_state.create () in
  let t = make_move_exn t (mv mono ~r:0 ~c:0) in
  print_decision t;
  let t = make_move_exn t (mv mono ~r:0 ~c:19) in
  print_decision t;
  let t = make_move_exn t (mv mono ~r:19 ~c:19) in
  print_decision t;
  let t = make_move_exn t (mv mono ~r:19 ~c:0) in
  print_decision t;
  pretty_print_board t ~rows:1;
  [%expect
    {|
    (In_progress (whose_turn Yellow))
    (In_progress (whose_turn Red))
    (In_progress (whose_turn Green))
    (In_progress (whose_turn Blue))
    B..................Y
    |}]
;;

let%expect_test "rules for later moves" =
  let t = four_corners () in
  (* overlapping an occupied square *)
  print_move_result (Game_state.make_move t (mv domino ~r:0 ~c:0));
  (* the 1-tile piece was already played *)
  print_move_result (Game_state.make_move t (mv mono ~r:1 ~c:1));
  (* side-by-side with Blue's own piece *)
  print_move_result (Game_state.make_move t (mv domino ~r:0 ~c:1));
  (* floating in the middle, touching nothing *)
  print_move_result (Game_state.make_move t (mv domino ~r:5 ~c:5));
  (* diagonal to Blue's piece: legal *)
  print_move_result (Game_state.make_move t (mv domino ~r:1 ~c:1));
  [%expect
    {|
    (Error Space_already_filled)
    (Error Piece_already_used)
    (Error Touches_own_edge)
    (Error No_corner_contact)
    (Ok (In_progress (whose_turn Yellow)))
    |}]
;;

let%expect_test "touching another color's side is allowed" =
  let t =
    with_board (Game_state.create ()) [ pos 0 0, Color.Blue; pos 1 2, Color.Yellow ]
  in
  (* (1,1) touches Blue's corner and Yellow's side *)
  print_validate (Game_state.validate_move t Blue (mv mono ~r:1 ~c:1));
  [%expect {| (Ok ()) |}]
;;

(* ===================================================================== *)
(* make_move results                                                      *)
(* ===================================================================== *)

let%expect_test "make_move returns a new state and leaves the old one alone" =
  let before = Game_state.create () in
  let after = make_move_exn before (mv mono ~r:0 ~c:0) in
  let blue (t : Game_state.t) = Game_state.find_player t Blue in
  printf "board squares: before %d, after %d\n" (Map.length before.board) (Map.length after.board);
  printf
    "Blue tiles: before %d, after %d\n"
    (Player.tiles_left (blue before))
    (Player.tiles_left (blue after));
  printf
    "Blue pieces: before %d, after %d\n"
    (List.length (blue before).remaining)
    (List.length (blue after).remaining);
  printf
    "1-tile piece still available: %b\n"
    (List.mem (blue after).remaining mono ~equal:Piece.equal);
  printf "last_move set: %b\n" (Option.is_some after.last_move);
  printf "old state unchanged: %b\n" (Game_state.equal before (Game_state.create ()));
  [%expect
    {|
    board squares: before 0, after 1
    Blue tiles: before 89, after 88
    Blue pieces: before 21, after 20
    1-tile piece still available: false
    last_move set: true
    old state unchanged: true
    |}]
;;

let%expect_test "no moves are allowed once the game is over" =
  let t = Game_state.create () in
  print_move_result
    (Game_state.make_move
       { t with Game_state.decision = Decision.Winner Color.Blue }
       (mv mono ~r:0 ~c:0));
  print_move_result
    (Game_state.make_move { t with Game_state.decision = Decision.Stalemate } (mv mono ~r:0 ~c:0));
  [%expect
    {|
    (Error Game_is_over)
    (Error Game_is_over)
    |}]
;;

(* ===================================================================== *)
(* Turns and the end of the game                                          *)
(* ===================================================================== *)

let%expect_test "players with no moves are skipped" =
  let t = with_remaining (Game_state.create ()) Yellow [] in
  print_move_result (Game_state.make_move t (mv mono ~r:0 ~c:0));
  [%expect {| (Ok (In_progress (whose_turn Red))) |}]
;;

let%expect_test "the last player who can move keeps moving" =
  let t = Game_state.create () in
  let t = with_remaining t Yellow [] in
  let t = with_remaining t Red [] in
  let t = with_remaining t Green [] in
  let t = with_remaining t Blue [ mono; domino ] in
  let t = make_move_exn t (mv mono ~r:0 ~c:0) in
  print_decision t;
  (* Blue plays the last piece; everyone has 0 tiles left *)
  print_move_result (Game_state.make_move t (mv domino ~r:1 ~c:1));
  [%expect
    {|
    (In_progress (whose_turn Blue))
    (Ok Stalemate)
    |}]
;;

let%expect_test "fewest tiles left wins when nobody can move" =
  (* Blue already owns the other three starting corners, so nobody else can start *)
  let t =
    with_board
      (Game_state.create ())
      [ pos 0 19, Color.Blue; pos 19 19, Color.Blue; pos 19 0, Color.Blue ]
  in
  let t = List.fold Color.all ~init:t ~f:(fun t color -> with_remaining t color [ mono ]) in
  printf "Yellow has a move: %b\n" (Game_state.has_any_move t Yellow);
  print_move_result (Game_state.make_move t (mv mono ~r:1 ~c:18));
  [%expect
    {|
    Yellow has a move: false
    (Ok (Winner Blue))
    |}]
;;

(* ===================================================================== *)
(* Random games                                                           *)
(* ===================================================================== *)

(* An independent implementation of the placement rules, used to list every
   legal move. Every move it finds must also pass Game_state.validate_move. *)
let legal_moves (t : Game_state.t) (color : Color.t) : Move.t list =
  let player = Game_state.find_player t color in
  let corner = Game_state.starting_corner t color in
  let first_move = not (Map.exists t.board ~f:(Color.equal color)) in
  let owned (sq : Coordinate.t) =
    Option.equal Color.equal (Map.find t.board sq) (Some color)
  in
  let empty_on_board (sq : Coordinate.t) =
    sq.r >= 0 && sq.r < t.rows && sq.c >= 0 && sq.c < t.columns && not (Map.mem t.board sq)
  in
  let edges (sq : Coordinate.t) =
    [ pos (sq.r - 1) sq.c; pos (sq.r + 1) sq.c; pos sq.r (sq.c - 1); pos sq.r (sq.c + 1) ]
  in
  let diagonals (sq : Coordinate.t) =
    [ pos (sq.r - 1) (sq.c - 1)
    ; pos (sq.r - 1) (sq.c + 1)
    ; pos (sq.r + 1) (sq.c - 1)
    ; pos (sq.r + 1) (sq.c + 1)
    ]
  in
  let is_legal (move : Move.t) =
    let squares = Move.squares move in
    List.for_all squares ~f:empty_on_board
    && (not (List.exists squares ~f:(fun sq -> List.exists (edges sq) ~f:owned)))
    &&
    if first_move
    then List.mem squares corner ~equal:Coordinate.equal
    else List.exists squares ~f:(fun sq -> List.exists (diagonals sq) ~f:owned)
  in
  let positions =
    List.concat_map (List.range 0 t.rows) ~f:(fun r ->
      List.map (List.range 0 t.columns) ~f:(fun c -> pos r c))
  in
  let moves =
    List.concat_map player.remaining ~f:(fun piece ->
      List.concat_map (Piece.orientations piece) ~f:(fun orientation ->
        List.filter_map positions ~f:(fun position ->
          let move : Move.t = { piece; orientation; position } in
          if is_legal move then Some move else None)))
  in
  List.iter moves ~f:(fun move ->
    check
      (Result.is_ok (Game_state.validate_move t color move))
      "validate_move rejected a legal move");
  moves
;;

let total_tiles = 89 * List.length Color.all

(* Everything that must be true after any successful move *)
let check_move ~(before : Game_state.t) ~(after : Game_state.t) ~(color : Color.t) (move : Move.t)
  =
  let size = Piece.size move.piece in
  check
    (List.for_all (Move.squares move) ~f:(fun sq ->
       Option.equal Color.equal (Map.find after.board sq) (Some color)))
    "placed squares are not the mover's color";
  check
    (Map.length after.board = Map.length before.board + size)
    "board did not grow by the piece size";
  check
    (Map.for_alli before.board ~f:(fun ~key ~data ->
       Option.equal Color.equal (Map.find after.board key) (Some data)))
    "a previously placed square changed";
  List.iter Color.all ~f:(fun c ->
    let tiles_before = Player.tiles_left (Game_state.find_player before c) in
    let tiles_after = Player.tiles_left (Game_state.find_player after c) in
    let expected = if Color.equal c color then tiles_before - size else tiles_before in
    check (tiles_after = expected) "tiles_left changed incorrectly");
  check
    (not
       (List.mem (Game_state.find_player after color).remaining move.piece ~equal:Piece.equal))
    "played piece is still in remaining";
  check
    (Map.length after.board + List.sum (module Int) after.players ~f:Player.tiles_left
     = total_tiles)
    "tiles on board + tiles in hand != 356";
  check (Option.equal Move.equal after.last_move (Some move)) "last_move not updated";
  match after.decision with
  | Decision.In_progress { whose_turn } ->
    check (Game_state.has_any_move after whose_turn) "next player has no legal move"
  | Decision.Winner _ | Decision.Stalemate -> ()
;;

(* Everything that must be true when the game ends *)
let check_finished (t : Game_state.t) =
  check
    (List.for_all Color.all ~f:(fun c -> not (Game_state.has_any_move t c)))
    "game ended but someone could still move";
  let scores = List.map t.players ~f:(fun (p : Player.t) -> p.color, Player.tiles_left p) in
  let best = List.fold scores ~init:Int.max_value ~f:(fun acc (_, s) -> Int.min acc s) in
  let leaders = List.filter scores ~f:(fun (_, s) -> s = best) |> List.map ~f:fst in
  (match t.decision with
   | Decision.In_progress _ -> check false "game is still in progress"
   | Decision.Winner c ->
     check (List.equal Color.equal leaders [ c ]) "winner does not have the fewest tiles"
   | Decision.Stalemate -> check (List.length leaders > 1) "stalemate with a unique leader");
  check
    (match Game_state.make_move t (mv mono ~r:0 ~c:0) with
     | Error Game_state.Move_error.Game_is_over -> true
     | _ -> false)
    "a move was accepted after the game ended"
;;

(* Plays a whole game with random legal moves. The seed makes it repeatable. *)
let random_walk ~seed =
  let random = Random.State.make [| seed |] in
  let rec loop (t : Game_state.t) moves_made =
    match t.decision with
    | Decision.Winner _ | Decision.Stalemate -> t, moves_made
    | Decision.In_progress { whose_turn } ->
      let moves = legal_moves t whose_turn in
      check (not (List.is_empty moves)) "player was given the turn with no legal moves";
      let move = List.nth_exn moves (Random.State.int random (List.length moves)) in
      let after = make_move_exn t move in
      check_move ~before:t ~after ~color:whose_turn move;
      loop after (moves_made + 1)
  in
  let final, moves_made = loop (Game_state.create ()) 0 in
  check_finished final;
  check (moves_made >= 4 && moves_made <= 84) "impossible number of moves";
  final, moves_made
;;

let%expect_test "random games end correctly and keep every invariant" =
  List.iter [ 1; 2; 3 ] ~f:(fun seed ->
    let (_ : Game_state.t * int) = random_walk ~seed in
    printf "seed %d: game finished, all checks passed\n" seed);
  [%expect
    {|
    seed 1: game finished, all checks passed
    seed 2: game finished, all checks passed
    seed 3: game finished, all checks passed
    |}]
;;

let%expect_test "the same seed plays the same game" =
  let game1, _ = random_walk ~seed:42 in
  let game2, _ = random_walk ~seed:42 in
  printf "identical: %b\n" (Game_state.equal game1 game2);
  [%expect {| identical: true |}]
;;

(* Keeps ok_exn used inside this file too *)
let%expect_test "ok_exn" =
  printf "%d\n" (ok_exn (Ok 5 : (int, string) Result.t));
  [%expect {| 5 |}]
;;