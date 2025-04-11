open Graphics
open Minesweeper.Board

(* Read a PPM file and return its dimensions and pixel array *)
let load_ppm filename : int * int * (int * int * int) array array =
  let ic = open_in_bin filename in
  let magic_number = input_line ic in
  if magic_number <> "P6" then failwith "Not a P6 PPM file";

  (* Skip comments *)
  let rec skip_comments () =
    let line = input_line ic in
    if String.length line > 0 && line.[0] = '#' then skip_comments () else line
  in

  let line = skip_comments () in
  let dimensions = Scanf.sscanf line "%d %d" (fun w h -> (w, h)) in
  let max_val = int_of_string (skip_comments ()) in
  if max_val > 255 then failwith "Only 8-bit PPM files are supported";

  let width, height = dimensions in
  let pixels =
    Array.init height (fun _ ->
        Array.init width (fun _ ->
            let r = input_byte ic in
            let g = input_byte ic in
            let b = input_byte ic in
            (r, g, b)))
  in
  close_in ic;
  (width, height, pixels)

let grid_size = ref 9 (* 9x9 grid *)
let grid_height = ref 3
let number_mines = ref 40
let square_size = ref (800 / !grid_size) (* Size of each square *)

let usage =
  "It appears you may not have entered a valid argument. You have three \
   options. \n\
   1. You can leave it blank for the default\n\
   2. You can enter: easy, medium, hard for the default size grid at varying \
   difficulties\n\
   3. You can enter: <difficulty> <grid size> <grid depth>. Note that \
   <grid_size> must be between 1 and 20 and height must be between 1 and 7"

let () =
  if Array.length Sys.argv = 1 then ()
  else if Array.length Sys.argv = 2 then (
    match Sys.argv.(1) with
    | "easy" -> number_mines := !grid_size * !grid_size * !grid_height / 18
    | "medium" -> number_mines := !grid_size * !grid_size * !grid_height / 6
    | "hard" -> number_mines := !grid_size * !grid_size * !grid_height / 3
    | _ ->
        print_endline usage;
        exit 0)
  else if Array.length Sys.argv = 4 then (
    let g_size = int_of_string Sys.argv.(2) in
    let h = int_of_string Sys.argv.(3) in
    grid_size :=
      if 0 < g_size && g_size < 21 then g_size
      else (
        print_endline "you inputted an unreasonably large number";
        exit 0);
    grid_height :=
      if 0 < h && h < 8 then g_size
      else (
        print_endline "you inputted an unreasonably large number";
        exit 0);
    square_size := 800 / !grid_size;
    match Sys.argv.(1) with
    | "easy" -> number_mines := !grid_size * !grid_size * !grid_height / 18
    | "medium" -> number_mines := !grid_size * !grid_size * !grid_height / 6
    | "hard" -> number_mines := !grid_size * !grid_size * !grid_height / 3
    | _ ->
        print_endline usage;
        exit 0)

let grid_size = !grid_size
let grid_height = !grid_height
let square_size = !square_size

type select_options =
  | Poke
  | Dig
  | Defuse
  | Flag

let selector = ref Poke
let board = create (grid_size, grid_size, grid_height)
let () = add_mines board !number_mines (*TODO: don't harcode*)
let mines_remaining = ref (count_mines board)
let defuses_remaining = ref !number_mines
let button_size = 100

(* Initialize the Graphics window *)
let () =
  let window_size = square_size * grid_size in
  open_graph
    (Printf.sprintf " %dx%d" (window_size + (2 * button_size)) window_size);
  set_window_title "Minesweeper";

  let draw_poke () =
    set_color black;
    (*poke button*)
    draw_rect
      (window_size + (button_size / 2))
      ((window_size / 2) + button_size)
      button_size button_size
  in
  let draw_dig () =
    set_color black;
    draw_rect
      (window_size + (button_size / 2))
      (window_size / 2) button_size button_size
  in

  let draw_defuse () =
    set_color black;
    draw_rect
      (window_size + (button_size / 2))
      ((window_size / 2) - button_size)
      button_size button_size
  in

  let draw_flag () =
    set_color black;
    draw_rect
      (window_size + (button_size / 2))
      ((window_size / 2) - (2 * button_size))
      button_size button_size
  in

  let draw_panel () =
    set_color (rgb 200 200 200);
    fill_rect window_size 0 (2 * button_size) window_size
  in

  let draw_indicator () =
    set_color red;
    draw_rect
      (window_size + (button_size / 2))
      ((window_size / 2) + (5 * button_size / 2))
      button_size button_size
  in

  let draw_num_mines () =
    set_color black;
    let str = "Mines: " ^ string_of_int !mines_remaining in
    moveto
      (window_size + (button_size / 2) - (String.length str / 2))
      button_size;
    draw_string str
  in
  let draw_defuses_left () =
    set_color black;
    let str2 = "Defuses: " ^ string_of_int !defuses_remaining in
    moveto
      (window_size + (button_size / 2) - (String.length str2 / 2))
      (button_size - 20);
    draw_string str2
  in

  (* Function to draw the grid *)
  let draw_grid () =
    for row = 0 to grid_size - 1 do
      for col = 0 to grid_size - 1 do
        let x = col * square_size in
        let y = row * square_size in
        set_color (rgb 51 153 51);
        (* Light green background *)
        fill_rect x y square_size square_size;
        set_color black;
        draw_rect x y square_size square_size
      done
    done;

    draw_panel ();
    draw_poke ();
    draw_dig ();
    draw_defuse ();
    draw_flag ();
    draw_indicator ();
    draw_num_mines ();
    draw_defuses_left ()
  in

  (* Draw initial grid *)
  draw_grid ();

  let is_button (x, y) (rect_x, rect_y) size =
    x >= rect_x && x <= rect_x + size && y >= rect_y && y <= rect_y + size
  in

  let update_selector (s : select_options) =
    selector := s (*TODO: change image*);
    match s with
    | Poke -> print_endline "poke"
    | Dig -> print_endline "dig"
    | Defuse -> print_endline "defuse"
    | Flag -> print_endline "flag"
  in

  let refresh_grid (ignore_win : bool) =
    draw_panel ();
    draw_poke ();
    draw_dig ();
    draw_defuse ();
    draw_flag ();
    draw_indicator ();
    draw_num_mines ();
    draw_defuses_left ();
    for i = 0 to grid_size - 1 do
      for j = 0 to grid_size - 1 do
        let block = get board (i, j) in
        if block = None then (
          set_color black;
          fill_rect (j * square_size) (i * square_size) square_size square_size)
        else
          let block = Option.get block in
          let r, g, b = Minesweeper.Block.color block in
          set_color (rgb r g b);
          fill_rect (j * square_size) (i * square_size) square_size square_size;
          set_color black;
          draw_rect (j * square_size) (i * square_size) square_size square_size;
          if Minesweeper.Block.is_num_visible block then (
            let text_x = (j * square_size) + (square_size / 2) in
            let text_y = (i * square_size) + (square_size / 2) - 5 in
            (* Adjust -5 for better centering *)
            moveto text_x text_y;
            draw_string
              (string_of_int (Minesweeper.Block.num_surrounding_mines block)))
          else ();
          if Minesweeper.Block.has_flag block then (
            set_color (rgb 255 0 0);
            fill_poly
              (* fill_circle ((col * square_size) + (square_size / 2)) ((row *
                 square_size) + (square_size / 2)) (square_size / 3); *)
              [|
                ( (j * square_size) + (square_size / 5),
                  (i * square_size) + (square_size / 5) );
                ( (j * square_size) + (square_size / 5),
                  (i * square_size) + (square_size * 4 / 5) );
                ( (j * square_size) + (square_size * 4 / 5),
                  (i * square_size) + (square_size / 2) );
              |];
            set_color black;
            draw_rect (j * square_size) (i * square_size) square_size
              square_size)
          else ()
      done
    done;

    if check_is_win board && not ignore_win then
      print_endline "How the hell did you win???"
  in

  let convert_to_graphics_color (pixels : (int * int * int) array array) :
      Graphics.color array array =
    Array.map
      (Array.map (fun (r, g, b) -> rgb r g b))
      (* Convert each (r, g, b) to Graphics.color *)
      pixels
  in

  let mine_clicked_animation msg =
    for k = 0 to grid_height do
      for j = grid_size downto 0 do
        for i = 0 to grid_size do
          let block = get board (i, j) in
          if
            block <> None
            && get_z board (i, j) = k
            && Minesweeper.Block.has_mine (Option.get block)
          then (
            set_color red;
            fill_rect (i * square_size) (j * square_size) square_size
              square_size;
            defuse board (i, j);
            let speed = 10.0 /. float_of_int !number_mines in
            Unix.sleepf speed)
          else ();
          if block <> None && get_z board (i, j) = k then dig board (i, j)
          else ()
        done
      done;
      refresh_grid true
    done;

    let width, height, pixel_arr = load_ppm "art/you_lose.ppm" in

    let img = make_image (convert_to_graphics_color pixel_arr) in

    Graphics.draw_image img 0 0;

    Unix.sleep 15;
    exit 0
  in

  let wompwomp_animation msg =
    let width, height, pixel_arr = load_ppm "art/wompwomp.ppm" in

    let img = make_image (convert_to_graphics_color pixel_arr) in

    Graphics.draw_image img 0 0;

    Unix.sleep 2
  in

  (* Function to handle square click and change color *)
  let handle_click x y =
    if x >= window_size then
      (*if poke button*)
      if
        is_button (x, y)
          (window_size + (button_size / 2), (window_size / 2) + button_size)
          button_size
      then update_selector Poke
      else if
        (*if dig button*)
        is_button (x, y)
          (window_size + (button_size / 2), window_size / 2)
          button_size
      then update_selector Dig
      else if
        (*if defuse button*)
        is_button (x, y)
          (window_size + (button_size / 2), (window_size / 2) - button_size)
          button_size
      then update_selector Defuse
      else if
        (*if flag button*)
        is_button (x, y)
          ( window_size + (button_size / 2),
            (window_size / 2) - (2 * button_size) )
          button_size
      then update_selector Flag
      else ()
    else
      let col = x / square_size in
      let row = y / square_size in
      if get board (row, col) = None then ()
      else (
        Printf.printf "Square at (%d, %d) clicked\n" col row;
        flush stdout;

        match !selector with
        | Poke -> (
            try poke board (row, col)
            with MineClicked msg -> mine_clicked_animation msg)
        | Dig -> (
            try dig board (row, col)
            with MineClicked msg -> mine_clicked_animation msg)
        | Defuse -> (
            if !defuses_remaining = 0 then ()
            else
              try
                defuse board (row, col);
                mines_remaining := !mines_remaining - 1;
                defuses_remaining := !defuses_remaining - 1
              with WompWomp ->
                wompwomp_animation "You defused a block without a mine :(")
        | Flag -> place_flag board (row, col));

      refresh_grid false
  in

  (* Event loop to capture clicks *)
  while true do
    let event = wait_next_event [ Button_down ] in
    if event.button then handle_click event.mouse_x event.mouse_y
  done

let draw_button label x y width height =
  set_color blue;
  fill_rect x y width height;
  set_color white;
  moveto (x + (width / 2) - (String.length label * 3)) (y + (height / 2) - 7);
  draw_string label
