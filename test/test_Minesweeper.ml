open OUnit2
open Minesweeper.Block

(***************************** BLOCK TESTS **********************************)
let make_block_create_test (x, y, z, mine, vis, c) : OUnit2.test list =
  let block = create (x, y, z, mine, vis) in
  [
    ( "create coordinates" >:: fun _ ->
      assert_equal (x, y, z) (coordinates block) );
    ("create mine" >:: fun _ -> assert_equal mine (has_mine block));
    ("create is_visible" >:: fun _ -> assert_equal vis (is_visible block));
    ("create color" >:: fun _ -> assert_equal c (color block));
  ]

let block_create_tests_cases =
  [
    (1, 2, 0, true, true, (51, 153, 51));
    (1, 2, 1, true, true, (91, 119, 0));
    (1, 2, 2, true, true, (106, 84, 0));
    (1, 2, 3, true, true, (102, 51, 0));
    (1, 2, 4, true, true, (84, 34, 16));
    (1, 2, 5, true, true, (61, 23, 21));
    (1, 2, 6, true, true, (35, 15, 17));
    (1, 2, 7, true, true, (0, 0, 0));
    (2, 3, 8, false, false, (0, 0, 0));
    (3, 4, 10, true, false, (0, 0, 0));
    (4, 5, 0, false, true, (51, 153, 51));
    (5, 6, 1, true, true, (91, 119, 0));
    (6, 7, 2, false, false, (106, 84, 0));
    (7, 8, 3, true, false, (102, 51, 0));
    (8, 9, 4, false, true, (84, 34, 16));
    (9, 10, 5, true, true, (61, 23, 21));
    (10, 11, 6, false, false, (35, 15, 17));
    (11, 12, 7, true, false, (0, 0, 0));
    (12, 13, 8, false, true, (0, 0, 0));
    (13, 14, 9, true, true, (0, 0, 0));
  ]

let rec block_tests_create t acc =
  match t with
  | [] -> acc
  | h :: t -> block_tests_create t (acc @ make_block_create_test h)

let block_create_tests = block_tests_create block_create_tests_cases []

let make_block_visibility_tests (input, expected_output) =
  "is_visible" >:: fun _ ->
  update_visibility expected_output input;
  assert_equal expected_output (is_visible input)

let block_vis_test_cases =
  [
    (create (1, 2, 3, true, true), true);
    (create (1, 2, 3, true, true), false);
    (create (1, 2, 3, true, false), true);
    (create (1, 2, 3, true, false), false);
    (create (3, 4, 5, false, false), true);
    (create (3, 4, 5, false, false), false);
    (create (7, 8, 9, true, false), true);
    (create (7, 8, 9, true, false), false);
    (create (10, 11, 12, false, true), true);
    (create (10, 11, 12, false, true), false);
    (create (14, 15, 16, true, true), true);
    (create (14, 15, 16, true, true), false);
    (create (20, 21, 22, false, false), true);
    (create (20, 21, 22, false, false), false);
    (create (25, 26, 27, true, false), true);
    (create (25, 26, 27, true, false), false);
    (create (30, 31, 32, true, true), true);
    (create (30, 31, 32, true, true), false);
    (create (40, 41, 42, false, false), true);
    (create (40, 41, 42, false, false), false);
    (create (50, 51, 52, true, true), true);
    (create (50, 51, 52, true, true), false);
    (create (60, 61, 62, false, true), true);
    (create (60, 61, 62, false, true), false);
    (create (70, 71, 72, true, false), true);
    (create (70, 71, 72, true, false), false);
    (create (80, 81, 82, false, true), true);
    (create (80, 81, 82, false, true), false);
    (create (90, 91, 92, true, true), true);
    (create (90, 91, 92, true, true), false);
  ]

let make_block_mine_tests (input, expected_output) =
  "has_mine" >:: fun _ ->
  update_mine expected_output input;
  assert_equal expected_output (has_mine input)

let block_mine_test_cases =
  [
    (create (1, 2, 3, true, true), true);
    (create (1, 2, 3, true, true), false);
    (create (1, 2, 3, false, true), true);
    (create (1, 2, 3, false, true), false);
    (create (2, 3, 4, false, false), true);
    (create (2, 3, 4, false, false), false);
    (create (5, 6, 7, true, false), true);
    (create (5, 6, 7, true, false), false);
    (create (9, 10, 11, false, true), true);
    (create (9, 10, 11, false, true), false);
    (create (12, 13, 14, true, true), true);
    (create (12, 13, 14, true, true), false);
    (create (15, 16, 17, false, false), true);
    (create (15, 16, 17, false, false), false);
    (create (18, 19, 20, true, false), true);
    (create (18, 19, 20, true, false), false);
    (create (21, 22, 23, false, true), true);
    (create (21, 22, 23, false, true), false);
    (create (24, 25, 26, true, false), true);
    (create (24, 25, 26, true, false), false);
    (create (30, 40, 50, true, false), true);
    (create (30, 40, 50, true, false), false);
    (create (35, 45, 55, false, true), true);
    (create (35, 45, 55, false, true), false);
    (create (40, 50, 60, true, false), true);
    (create (40, 50, 60, true, false), false);
  ]

let make_block_flag_tests (input, expected_output) =
  "has_flag" >:: fun _ ->
  update_flag expected_output input;
  assert_equal expected_output (has_flag input)

let block_flag_test_cases =
  [
    (create (1, 2, 3, true, true), true);
    (create (1, 2, 3, true, true), false);
    (create (2, 3, 4, false, true), true);
    (create (2, 3, 4, false, true), false);
    (create (5, 6, 7, true, false), true);
    (create (5, 6, 7, true, false), false);
    (create (9, 10, 11, false, true), true);
    (create (9, 10, 11, false, true), false);
    (create (12, 13, 14, true, true), true);
    (create (12, 13, 14, true, true), false);
    (create (15, 16, 17, false, false), true);
    (create (15, 16, 17, false, false), false);
    (create (18, 19, 20, true, false), true);
    (create (18, 19, 20, true, false), false);
    (create (21, 22, 23, false, true), true);
    (create (21, 22, 23, false, true), false);
    (create (24, 25, 26, true, true), true);
    (create (24, 25, 26, true, true), false);
    (create (30, 31, 32, true, false), true);
    (create (30, 31, 32, true, false), false);
    (create (33, 34, 35, false, true), true);
    (create (33, 34, 35, false, true), false);
    (create (40, 50, 60, true, false), true);
    (create (40, 50, 60, true, false), false);
    (create (45, 55, 65, false, true), true);
    (create (45, 55, 65, false, true), false);
  ]

let block_vis_tests = List.map make_block_visibility_tests block_vis_test_cases
let block_mine_tests = List.map make_block_mine_tests block_mine_test_cases
let block_flag_tests = List.map make_block_flag_tests block_flag_test_cases

let block_tests =
  block_create_tests @ block_vis_tests @ block_mine_tests @ block_flag_tests

let _ = run_test_tt_main ("block test suite" >::: block_tests)

(******************************** BOARD TESTS ********************************)
open Minesweeper.Board

let make_board_rows_test (x, y, z) =
  let board = create (x, y, z) in
  "rows" >:: fun _ -> assert_equal x (rows board)

let make_board_cols_test (x, y, z) =
  let board = create (x, y, z) in
  "cols" >:: fun _ -> assert_equal y (cols board)

let make_board_height_test (x, y, z) =
  let board = create (x, y, z) in
  "height" >:: fun _ -> assert_equal z (height board)

let board_dim_test_cases =
  [
    (1, 2, 3);
    (1, 1, 1);
    (10, 1, 1);
    (90, 23, 4);
    (3, 2, 1);
    (9, 4, 3);
    (2, 6, 5);
    (7, 7, 8);
    (15, 15, 15);
    (100, 100, 1);
    (50, 50, 50);
    (20, 5, 10);
    (5, 5, 5);
    (30, 40, 50);
    (12, 12, 12);
    (8, 16, 24);
    (60, 10, 5);
    (25, 25, 25);
    (99, 99, 3);
  ]

let board_row_tests = List.map make_board_rows_test board_dim_test_cases
let board_col_tests = List.map make_board_cols_test board_dim_test_cases
let board_height_tests = List.map make_board_height_test board_dim_test_cases
let board_dim_tests = board_row_tests @ board_col_tests @ board_height_tests
let _ = run_test_tt_main ("test suite" >::: board_dim_tests)
let cmp_x_y (x, y) (x', y', _) = x = x' && y = y'

let make_board_get_test (r, c, h, x, y) =
  let board = create (r, c, h) in
  let block = Option.get (get board (x, y)) in
  "get" >:: fun _ -> assert_equal true (cmp_x_y (x, y) (coordinates block))

let board_get_test_cases =
  [
    (1, 2, 3, 0, 0);
    (1, 1, 1, 0, 0);
    (10, 1, 1, 6, 0);
    (90, 23, 4, 12, 21);
    (3, 2, 1, 2, 1);
    (9, 4, 3, 8, 1);
    (2, 6, 5, 1, 5);
    (7, 7, 8, 6, 0);
    (15, 15, 15, 14, 14);
    (20, 20, 20, 19, 19);
    (12, 12, 12, 0, 0);
    (8, 8, 8, 7, 7);
    (50, 50, 50, 25, 25);
    (10, 10, 10, 9, 9);
    (5, 5, 5, 4, 4);
    (6, 12, 18, 5, 10);
    (25, 25, 25, 24, 24);
    (4, 16, 64, 3, 3);
    (7, 7, 7, 6, 6);
    (30, 30, 30, 29, 29);
    (40, 40, 40, 39, 39);
    (3, 3, 3, 2, 2);
    (15, 30, 45, 14, 14);
    (33, 33, 33, 30, 30);
    (2, 4, 6, 1, 1);
    (14, 14, 14, 13, 13);
    (60, 60, 60, 59, 59);
    (18, 18, 18, 17, 17);
    (77, 77, 77, 76, 76);
    (100, 100, 100, 99, 99);
    (24, 24, 24, 23, 23);
    (6, 18, 54, 5, 17);
    (50, 25, 10, 49, 24);
    (20, 40, 60, 19, 39);
    (9, 27, 81, 8, 26);
  ]

let board_get_tests = List.map make_board_get_test board_get_test_cases
let _ = run_test_tt_main ("test suite" >::: board_get_tests)

let make_board_place_flag_test (r, c, h, x, y) =
  let board = create (r, c, h) in
  place_flag board (x, y);
  let block = Option.get (get board (x, y)) in
  "get" >:: fun _ -> assert_equal true (has_flag block)

let board_place_flag_test_cases =
  [
    (1, 3, 4, 0, 1);
    (1, 6, 1, 0, 0);
    (11, 4, 1, 6, 0);
    (59, 33, 4, 12, 31);
    (5, 2, 1, 0, 1);
    (9, 6, 3, 8, 2);
    (4, 7, 5, 1, 5);
    (7, 9, 8, 5, 0);
    (15, 15, 15, 10, 10);
    (20, 20, 20, 19, 19);
    (12, 12, 12, 0, 0);
    (8, 8, 8, 7, 7);
    (50, 50, 50, 25, 25);
    (10, 10, 10, 9, 0);
    (5, 5, 5, 4, 4);
    (6, 12, 18, 5, 10);
    (25, 25, 25, 24, 24);
    (4, 16, 64, 3, 3);
    (7, 7, 7, 6, 6);
    (30, 30, 30, 29, 29);
    (40, 40, 40, 39, 39);
    (3, 3, 3, 2, 2);
    (15, 30, 45, 14, 14);
    (33, 33, 33, 30, 30);
    (2, 4, 6, 1, 1);
    (14, 14, 14, 13, 13);
    (60, 60, 60, 59, 59);
    (18, 18, 18, 17, 17);
  ]

let board_place_flag_tests =
  List.map make_board_place_flag_test board_place_flag_test_cases

let _ = run_test_tt_main ("test suite" >::: board_place_flag_tests)

let make_board_dig_z_test (r, c, h, x, y) =
  let board = create (r, c, h) in
  let z = get_z board (x, y) in
  dig board (x, y);
  let z' = get_z board (x, y) in
  if h = 1 then "dig z" >:: fun _ -> assert_equal (-1) z'
  else "dig z" >:: fun _ -> assert_equal (z + 1) z'

let make_board_dig_vis_test (r, c, h, x, y) =
  let board = create (r, c, h) in
  dig board (x, y);
  let block = get board (x, y) in
  if h = 1 then "dig vis" >:: fun _ -> assert_equal None block
  else "dig vis" >:: fun _ -> assert_equal true (is_visible (Option.get block))

let make_board_dig_exn_test (r, c, h, x, y) =
  let board = create (r, c, h) in
  let block = Option.get (get board (x, y)) in
  update_mine true block;
  "dig exn" >:: fun _ ->
  assert_raises
    (MineClicked (Printf.sprintf "Mine clicked at: (%d,%d)" x y))
    (fun _ -> dig board (x, y))

let board_dig_test_cases =
  [
    (1, 3, 4, 0, 1);
    (4, 6, 1, 0, 3);
    (11, 4, 1, 6, 0);
    (59, 33, 12, 12, 31);
    (5, 2, 7, 0, 1);
    (9, 6, 15, 8, 2);
    (4, 7, 5, 1, 5);
    (7, 5, 8, 5, 0);
    (15, 15, 15, 10, 10);
    (20, 20, 20, 19, 19);
    (12, 12, 12, 0, 0);
    (8, 8, 8, 7, 7);
    (50, 50, 50, 25, 25);
    (10, 10, 10, 9, 9);
    (5, 5, 5, 4, 4);
    (6, 12, 18, 5, 10);
    (25, 25, 25, 24, 24);
    (4, 16, 64, 3, 3);
    (7, 7, 7, 6, 6);
    (30, 30, 30, 29, 29);
    (40, 40, 40, 39, 39);
    (3, 3, 3, 2, 2);
    (15, 30, 45, 14, 14);
    (33, 33, 33, 30, 30);
    (2, 4, 6, 1, 1);
    (14, 14, 14, 13, 13);
    (60, 60, 60, 59, 59);
    (18, 18, 18, 17, 17);
    (77, 77, 77, 76, 76);
    (100, 100, 100, 99, 99);
    (24, 24, 24, 23, 23);
    (6, 18, 54, 5, 17);
    (50, 25, 10, 49, 24);
    (20, 40, 60, 19, 39);
  ]

let board_dig_z_tests = List.map make_board_dig_z_test board_dig_test_cases
let board_dig_vis_tests = List.map make_board_dig_vis_test board_dig_test_cases
let board_dig_exn_tests = List.map make_board_dig_exn_test board_dig_test_cases

let board_dig_tests =
  board_dig_z_tests @ board_dig_vis_tests @ board_dig_exn_tests

let _ = run_test_tt_main ("test suite" >::: board_dig_tests)

let make_board_defuse_good_test (r, c, h, x, y) =
  let board = create (r, c, h) in
  let block = Option.get (get board (x, y)) in
  update_mine true block;
  defuse board (x, y);
  "defuse" >:: fun _ -> assert_equal false (has_mine block)

let make_board_defuse_exn_test (r, c, h, x, y) =
  let board = create (r, c, h) in
  let block = Option.get (get board (x, y)) in
  update_mine false block;
  "defuse exn" >:: fun _ ->
  assert_raises WompWomp (fun _ -> defuse board (x, y))

let board_defuse_test_cases =
  [
    (1, 3, 3, 0, 1);
    (4, 6, 1, 0, 3);
    (11, 4, 1, 6, 0);
    (59, 33, 15, 12, 31);
    (5, 3, 7, 0, 1);
    (12, 7, 15, 5, 2);
    (4, 7, 5, 1, 5);
    (6, 5, 8, 5, 2);
    (15, 15, 15, 10, 10);
    (10, 10, 10, 0, 0);
    (20, 20, 20, 19, 19);
    (8, 8, 8, 7, 7);
    (6, 6, 6, 5, 5);
    (50, 50, 50, 25, 25);
    (30, 30, 30, 10, 10);
    (25, 25, 25, 24, 24);
    (12, 15, 18, 11, 14);
    (9, 27, 81, 8, 26);
    (7, 14, 21, 6, 13);
    (3, 9, 27, 2, 8);
    (45, 45, 45, 44, 44);
    (70, 70, 70, 69, 69);
    (88, 88, 88, 87, 87);
    (1, 100, 100, 0, 99);
    (90, 90, 90, 89, 89);
  ]

let board_defuse_good_tests =
  List.map make_board_defuse_good_test board_defuse_test_cases

let board_defuse_exn_tests =
  List.map make_board_defuse_exn_test board_defuse_test_cases

let board_defuse_tests = board_defuse_good_tests @ board_defuse_exn_tests
let _ = run_test_tt_main ("test suite" >::: board_defuse_tests)

let make_board_mine_test (r, c, h, num_mines) =
  let board = create (r, c, h) in
  add_mines board num_mines;
  "mine num" >:: fun _ -> assert_equal true (count_mines board <= num_mines)

let board_mine_test_cases =
  [
    (1, 3, 3, 2);
    (4, 6, 1, 5);
    (11, 4, 1, 6);
    (59, 33, 15, 12);
    (5, 3, 7, 0);
    (12, 7, 15, 5);
    (4, 7, 5, 1);
    (6, 5, 8, 5);
    (15, 15, 15, 50);
    (20, 20, 20, 15);
    (10, 10, 10, 8);
    (8, 8, 8, 4);
    (6, 6, 6, 3);
    (50, 50, 50, 40);
    (30, 30, 30, 25);
    (25, 25, 25, 10);
    (12, 12, 12, 5);
    (9, 27, 81, 20);
    (7, 14, 21, 10);
    (3, 9, 27, 15);
    (70, 70, 70, 35);
    (88, 88, 88, 40);
    (1, 100, 100, 50);
    (45, 45, 45, 20);
    (90, 90, 90, 60);
  ]

let board_mine_tests = List.map make_board_mine_test board_mine_test_cases
let _ = run_test_tt_main ("test suite" >::: board_mine_tests)
