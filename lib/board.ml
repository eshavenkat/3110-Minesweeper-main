open Block

type t = Block.t option array array array

exception MineClicked of string
exception WompWomp

let rows (b : t) =
  (* print_endline ("rows: " ^ string_of_int (Array.length b)); *)
  Array.length b

let cols (b : t) =
  (* print_endline ("cols: " ^ string_of_int (Array.length b.(0))); *)
  Array.length b.(0)

let height (b : t) =
  (* print_endline ("height: " ^ string_of_int (Array.length b.(0).(0))); *)
  Array.length b.(0).(0)

let get (b : t) (x, y) =
  if x >= rows b || y >= cols b then None
  else
    let rec inner (b : t) (x, y) h =
      if h >= height b then None
      else
        match b.(x).(y).(h) with
        | None -> inner b (x, y) (h + 1)
        | Some block -> Some block
    in
    inner b (x, y) 0

let get_z (b : t) (x, y) =
  let rec inner (b : t) (x, y) h =
    if h >= height b then -1
    else
      match b.(x).(y).(h) with
      | None -> inner b (x, y) (h + 1)
      | Some block -> h
  in
  inner b (x, y) 0

let init (b : t) =
  for r = 0 to rows b - 1 do
    for c = 0 to cols b - 1 do
      for h = 0 to height b - 1 do
        b.(r).(c).(h) <- Some (Block.create (r, c, h, false, false))
      done
    done
  done;
  for i = 0 to rows b - 1 do
    for j = 0 to cols b - 1 do
      Block.update_visibility true (Option.get b.(i).(j).(0))
    done
  done

let update_surrounding_blocks (delta : int) (b : t) (x, y, z) =
  for i = x - 1 to x + 1 do
    for j = y - 1 to y + 1 do
      for k = z - 1 to z + 1 do
        if
          0 <= i
          && i < rows b
          && 0 <= j
          && j < cols b
          && 0 <= k
          && k < height b (*Block with mine is also being updated*)
        then
          match b.(i).(j).(k) with
          | None -> ()
          | Some block -> Block.update_num_surrounding_mines delta block
        else ()
      done
    done
  done

let add_mines (b : t) (num_mines : int) =
  let () = Random.self_init () in
  for i = 0 to num_mines - 1 do
    let r = Random.int (rows b) in
    let c = Random.int (cols b) in
    let h = Random.int (height b) in
    Block.update_mine true (Option.get b.(r).(c).(h));
    update_surrounding_blocks 1 b (r, c, h)
  done

(** Reveal all blocks in the game board, typically used for game over scenarios *)
let reveal_all_blocks (b : t) =
  for i = 0 to rows b - 1 do
    for j = 0 to cols b - 1 do
      for k = 0 to height b - 1 do
        match b.(i).(j).(k) with
        | None -> ()
        | Some block -> Block.update_visibility true block
      done
    done
  done

(** Create a hint system that reveals a safe block *)
let get_safe_hint (b : t) =
  let rec find_safe_block () =
    let r = Random.int (rows b) in
    let c = Random.int (cols b) in
    let h = Random.int (height b) in
    match b.(r).(c).(h) with
    | None -> find_safe_block ()
    | Some block ->
        if (not (has_mine block)) && not (is_visible block) then Some (r, c)
        else find_safe_block ()
  in
  find_safe_block ()

type game_score = {
  total_mines : int;
  flags_placed : int;
  blocks_revealed : int;
  time_elapsed : float;
  score : int;
}

(** Calculate game score based on time and actions *)

(**THIS FUNCTION CAN ONLY BE CALLED AT THE BEGINNING PLSSS*)
let count_mines (b : t) =
  let num = ref 0 in
  for i = 0 to rows b - 1 do
    for j = 0 to cols b - 1 do
      for k = 0 to height b - 1 do
        let block = b.(i).(j).(k) in
        if block = None then ()
        else if has_mine (Option.get block) then num := !num + 1
        else ()
      done
    done
  done;
  !num

let create (x, y, z) =
  let a = Array.init x (fun _ -> Array.init y (fun _ -> Array.make z None)) in
  let () = init a in
  a

let place_flag (b : t) (x, y) =
  let block = get b (x, y) in
  if block = None then ()
  else update_flag (not (has_flag (Option.get block))) (Option.get block)

let poke_block (b : t) (x, y) =
  let block = Option.get (get b (x, y)) in
  if has_mine block then
    raise (MineClicked (Printf.sprintf "Mine clicked at: (%d,%d)" x y))
  else update_num_visible true block;
  print_endline (string_of_int (num_surrounding_mines block));
  print_endline (string_of_bool (is_num_visible block))

let rec poke_around (b : t) (x, y, z) =
  for i = x - 1 to x + 1 do
    for j = y - 1 to y + 1 do
      for k = z - 1 to z + 1 do
        if
          0 <= i
          && i < rows b
          && 0 <= j
          && j < cols b
          && 0 <= k
          && k < height b
          && not (i = x && j = y && k = z)
        then
          match b.(i).(j).(k) with
          | None -> ()
          | Some blc ->
              if is_visible blc && not (is_num_visible blc) then (
                poke_block b (i, j);
                if num_surrounding_mines blc = 0 then (
                  print_endline "calling poke_around";
                  poke_around b (i, j, k))
                else ())
              else ()
      done
    done
  done

let poke (b : t) (x, y) =
  poke_block b (x, y);
  let block = Option.get (get b (x, y)) in
  if num_surrounding_mines block = 0 then
    let z = get_z b (x, y) in
    poke_around b (x, y, z)
  else ()

let dig_block (b : t) (x, y) =
  let block = Option.get (get b (x, y)) in
  if has_mine block then
    raise (MineClicked (Printf.sprintf "Mine clicked at: (%d,%d)" x y))
  else
    let z = get_z b (x, y) in
    b.(x).(y).(z) <- None;
    let block' = get b (x, y) in
    match block' with
    | None -> ()
    | Some bl -> update_visibility true bl

let rec dig_around (b : t) (x, y, z) =
  for i = x - 1 to x + 1 do
    for j = y - 1 to y + 1 do
      for k = z - 1 to z + 1 do
        if
          0 <= i
          && i < rows b
          && 0 <= j
          && j < cols b
          && 0 <= k
          && k < height b
          && not (i = x && j = y && k = z)
        then
          match b.(i).(j).(k) with
          | None -> ()
          | Some blc ->
              if
                is_visible blc && is_num_visible blc
                && num_surrounding_mines blc = 0
              then (
                dig_block b (i, j);
                print_endline "calling dig_around";
                dig_around b (i, j, k))
              else ()
      done
    done
  done

let dig (b : t) (x, y) =
  let block = get b (x, y) in
  if block = None then ()
  else
    let block = Option.get block in
    let num = num_surrounding_mines block in
    let z = get_z b (x, y) in
    dig_block b (x, y);
    if num = 0 then dig_around b (x, y, z) else ()

let defuse (b : t) (x, y) =
  let block = get b (x, y) in
  if block = None then ()
  else
    let block = Option.get block in
    let z = get_z b (x, y) in
    if has_mine block then (
      update_mine false block;
      update_surrounding_blocks (-1) b (x, y, z))
    else raise WompWomp

let check_is_win (b : t) : bool =
  let win = ref true in
  for i = 0 to rows b - 1 do
    for j = 0 to cols b - 1 do
      for k = 0 to height b - 1 do
        if not !win then ()
        else if
          b.(i).(j).(k) <> None && not (has_mine (Option.get b.(i).(j).(k)))
        then win := false
      done
    done
  done;
  !win

let num_flags_placed (b : t) =
  let count = ref 0 in
  for i = 0 to rows b - 1 do
    for j = 0 to cols b - 1 do
      for k = 0 to height b - 1 do
        match b.(i).(j).(k) with
        | Some block when Block.has_flag block -> incr count
        | _ -> ()
      done
    done
  done;
  !count

let calculate_score ~start_time ~end_time (b : t) total_mines =
  let blocks_revealed =
    let count = ref 0 in
    for i = 0 to rows b - 1 do
      for j = 0 to cols b - 1 do
        for k = 0 to height b - 1 do
          match b.(i).(j).(k) with
          | Some block when is_visible block -> incr count
          | _ -> ()
        done
      done
    done;
    !count
  in
  let time_elapsed = end_time -. start_time in
  let base_score = blocks_revealed * 10 in
  let time_bonus =
    if time_elapsed < 60.0 then 500
    else if time_elapsed < 300.0 then 250
    else 100
  in
  let flags_penalty = num_flags_placed b * -5 in
  {
    total_mines;
    flags_placed = num_flags_placed b;
    blocks_revealed;
    time_elapsed;
    score = base_score + time_bonus + flags_penalty;
  }

(** Advanced mine placement strategy with minimum safe zone *)
let add_mines_with_safe_zone (b : t) (num_mines : int) =
  let () = Random.self_init () in
  let safe_radius = max 1 (min (rows b) (cols b) / 5) in

  (* Initial safe zone selection *)
  let safe_center_x = rows b / 2 in
  let safe_center_y = cols b / 2 in

  let rec place_mine_safely attempts =
    if attempts > num_mines * 10 then
      failwith "Could not place mines safely after many attempts"
    else
      let r = Random.int (rows b) in
      let c = Random.int (cols b) in
      let h = Random.int (height b) in

      (* Check if proposed mine is outside safe zone *)
      let distance =
        sqrt
          (float_of_int
             (((r - safe_center_x) * (r - safe_center_x))
             + ((c - safe_center_y) * (c - safe_center_y))))
      in

      if distance >= float_of_int safe_radius then
        match b.(r).(c).(h) with
        | Some block when not (has_mine block) ->
            Block.update_mine true block;
            update_surrounding_blocks 1 b (r, c, h)
        | _ -> place_mine_safely (attempts + 1)
      else place_mine_safely (attempts + 1)
  in

  for _ = 1 to num_mines do
    place_mine_safely 0
  done

(** Serialization function to save game state *)
let serialize_game_state (b : t) : string =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer
    (Printf.sprintf "%d:%d:%d\n" (rows b) (cols b) (height b));

  for i = 0 to rows b - 1 do
    for j = 0 to cols b - 1 do
      for k = 0 to height b - 1 do
        match b.(i).(j).(k) with
        | None -> Buffer.add_string buffer "N:"
        | Some block ->
            Buffer.add_string buffer
              (Printf.sprintf "B:%b:%b:%b:%d\n" (has_mine block)
                 (has_flag block) (is_visible block)
                 (num_surrounding_mines block))
      done
    done
  done;

  Buffer.contents buffer

let restart_game (b : t) =
  for i = 0 to rows b - 1 do
    for j = 0 to cols b - 1 do
      for k = 0 to height b - 1 do
        b.(i).(j).(k) <- Some (Block.create (i, j, k, false, false))
      done
    done
  done;
  add_mines b (count_mines b)

let num_remaining_mines (b : t) total_mines = total_mines - num_flags_placed b
let previous_states = ref []

let save_state (b : t) =
  previous_states := Array.map (Array.map Array.copy) b :: !previous_states

let reveal_hint (b : t) =
  match get_safe_hint b with
  | None -> ()
  | Some (x, y) ->
      let block = Option.get (get b (x, y)) in
      update_visibility true block

let save_game (b : t) filename =
  let oc = open_out_bin filename in
  output_string oc (serialize_game_state b);
  close_out oc

let load_game filename =
  let ic = open_in_bin filename in
  let game_state = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let lines = String.split_on_char '\n' game_state in
  let dimensions = String.split_on_char ':' (List.hd lines) in
  let x = int_of_string (List.nth dimensions 0) in
  let y = int_of_string (List.nth dimensions 1) in
  let z = int_of_string (List.nth dimensions 2) in

  let b = create (x, y, z) in
  List.iteri
    (fun i line ->
      if i > 0 then
        let parts = String.split_on_char ':' line in
        match List.hd parts with
        | "N" -> ()
        | "B" ->
            let r = i / (y * z) in
            let c = i mod (y * z) / z in
            let h = i mod (y * z) mod z in
            b.(r).(c).(h) <-
              Some
                (Block.create
                   ( r,
                     c,
                     h,
                     bool_of_string (List.nth parts 1),
                     bool_of_string (List.nth parts 2) ));
            let block = Option.get b.(r).(c).(h) in
            update_flag (bool_of_string (List.nth parts 2)) block;
            update_num_surrounding_mines
              (int_of_string (List.nth parts 3))
              block
        | _ -> ())
    lines;
  b

let display_board (b : t) =
  for i = 0 to rows b - 1 do
    for j = 0 to cols b - 1 do
      for k = 0 to height b - 1 do
        match b.(i).(j).(k) with
        | None -> print_string "[ ]"
        | Some block ->
            if is_visible block then
              if has_mine block then print_string "[*]"
              else if is_num_visible block then
                Printf.printf "[%d]" (num_surrounding_mines block)
              else print_string "[_]"
            else if has_flag block then print_string "[F]"
            else print_string "[#]"
      done;
      print_newline ()
    done;
    print_newline ()
  done

let game_paused = ref false
let pause_game () = game_paused := true
let resume_game () = game_paused := false

let highlight_surrounding_mines (b : t) (x, y, z) =
  for i = x - 1 to x + 1 do
    for j = y - 1 to y + 1 do
      for k = z - 1 to z + 1 do
        if
          0 <= i
          && i < rows b
          && 0 <= j
          && j < cols b
          && 0 <= k
          && k < height b
          && not (i = x && j = y && k = z)
        then
          match b.(i).(j).(k) with
          | None -> ()
          | Some block -> update_visibility true block
      done
    done
  done

let hint_safe_move (b : t) =
  match get_safe_hint b with
  | None -> ()
  | Some (x, y) -> Printf.printf "Hint: Try moving to (%d, %d)\n" x y

let debug_mode = ref false
let toggle_debug_mode () = debug_mode := not !debug_mode

let display_debug_board (b : t) =
  if !debug_mode then display_board b else print_endline "Debug mode is off"

type game_stats = {
  mutable games_played : int;
  mutable games_won : int;
  mutable games_lost : int;
}

let stats = { games_played = 0; games_won = 0; games_lost = 0 }

let update_stats ~won =
  stats.games_played <- stats.games_played + 1;
  if won then stats.games_won <- stats.games_won + 1
  else stats.games_lost <- stats.games_lost + 1

let display_stats () =
  Printf.printf "Games Played: %d\nGames Won: %d\nGames Lost: %d\n"
    stats.games_played stats.games_won stats.games_lost

let adaptive_difficulty (b : t) difficulty_level =
  let num_mines =
    match difficulty_level with
    | "easy" -> rows b * cols b * height b / 10
    | "medium" -> rows b * cols b * height b / 8
    | "hard" -> rows b * cols b * height b / 5
    | _ -> rows b * cols b * height b / 8
  in
  add_mines b num_mines

type leaderboard_entry = {
  player_name : string;
  score : int;
}

let leaderboard = ref []

let add_to_leaderboard player_name score =
  leaderboard := { player_name; score } :: !leaderboard;
  leaderboard := List.sort (fun a b -> compare b.score a.score) !leaderboard

let display_leaderboard () =
  Printf.printf "Leaderboard:\n";
  List.iteri
    (fun i entry ->
      Printf.printf "%d. %s - %d\n" (i + 1) entry.player_name entry.score)
    !leaderboard

let rec game_loop (b : t) =
  print_endline "Enter a command (poke, dig, flag, restart, quit):";
  let command = read_line () in
  match String.split_on_char ' ' command with
  | [ "poke"; x; y ] ->
      poke b (int_of_string x, int_of_string y);
      game_loop b
  | [ "dig"; x; y ] ->
      dig b (int_of_string x, int_of_string y);
      game_loop b
  | [ "flag"; x; y ] ->
      place_flag b (int_of_string x, int_of_string y);
      game_loop b
  | [ "restart" ] ->
      restart_game b;
      game_loop b
  | [ "quit" ] -> print_endline "Goodbye!"
  | _ ->
      print_endline "Invalid command.";
      game_loop b

let moves = ref 0
let increment_moves () = moves := !moves + 1
let get_moves () = !moves
let hints = ref 3

let use_hint (b : t) =
  if !hints > 0 then (
    decr hints;
    reveal_hint b)
  else print_endline "No hints remaining"

let highlight_safe_blocks (b : t) =
  for i = 0 to rows b - 1 do
    for j = 0 to cols b - 1 do
      for k = 0 to height b - 1 do
        match b.(i).(j).(k) with
        | None -> ()
        | Some block ->
            if not (has_mine block) then Block.update_visibility true block
      done
    done
  done;
  ignore (read_line ());
  (* Wait for user to press Enter *)
  for i = 0 to rows b - 1 do
    for j = 0 to cols b - 1 do
      for k = 0 to height b - 1 do
        match b.(i).(j).(k) with
        | None -> ()
        | Some block ->
            if not (has_mine block) then Block.update_visibility false block
      done
    done
  done

type timer = {
  mutable start_time : float;
  mutable elapsed : float;
  mutable running : bool;
}

let game_timer = { start_time = 0.0; elapsed = 0.0; running = false }

let start_timer () =
  game_timer.start_time <- Sys.time ();
  game_timer.running <- true

let pause_timer () =
  if game_timer.running then (
    game_timer.elapsed <-
      game_timer.elapsed +. (Sys.time () -. game_timer.start_time);
    game_timer.running <- false)
