(* module type BlockSig = sig type t (**This represents the type of a block*)

   val coordinates : t -> int * int * int (**[coordinates] is the [(x,y,z)]
   location of the block*)

   val mine : t -> bool (**[mine t] is the state of whether or not the block,
   [t] contains a mine, [true] is it contains a mine*)

   val is_visible : t -> bool (**[is_visible t] is whether or not the block [t]
   can be seen. [false] if it has been dug up or is under another block*)

   val color : t -> int * int * int (**[color t] is the color of the block, [t]
   based on its z-coordinate*)

   val update_visibility : bool -> t -> t (**[update_visibility b t] updates
   whether or not the block, [t], is visible with the value of [b]*)

   val create : int * int * int * bool * bool -> t (**[create (x, y, z, mine,
   vis)] is the block based on the [x],[y],[z] coordinates and whether there is
   a mine is equal to [mine] and whether it is visible equal to [vis]*) end *)

(* module Block : BlockSig = struct *)
type t = {
  x : int;
  y : int;
  z : int;
  mutable has_mine : bool;
  mutable is_visible : bool;
  mutable has_flag : bool;
  mutable num_surrounding_mines : int;
  mutable is_num_visible : bool;
}

(* GETTERS *)
let get_x (block : t) = block.x
let get_y (block : t) = block.y
let get_z (block : t) = block.z
let get_has_mine (block : t) = block.has_mine
let get_is_visible (block : t) = block.is_visible
let get_has_flag (block : t) = block.has_flag
let get_num_surrounding_mines (block : t) = block.num_surrounding_mines
let get_is_num_visible (block : t) = block.is_num_visible
let coordinates (block : t) = (get_x block, get_y block, get_z block)
let has_mine (block : t) = get_has_mine block
let is_visible (block : t) = get_is_visible block
let has_flag (block : t) = get_has_flag block
let is_num_visible (block : t) = get_is_num_visible block
let num_surrounding_mines (block : t) = get_num_surrounding_mines block

let color (block : t) =
  let z = get_z block in
  match z with
  | 0 -> (51, 153, 51)
  | 1 -> (91, 119, 0)
  | 2 -> (106, 84, 0)
  | 3 -> (102, 51, 0)
  | 4 -> (84, 34, 16)
  | 5 -> (61, 23, 21)
  | 6 -> (35, 15, 17)
  | _ -> (40, 40, 40)

(* Setters *)
let set_is_visible (b : bool) (block : t) = block.is_visible <- b
let set_has_mine (b : bool) (block : t) = block.has_mine <- b
let set_has_flag (b : bool) (block : t) = block.has_flag <- b

let set_num_surrounding_mines (i : int) (block : t) =
  block.num_surrounding_mines <- i + block.num_surrounding_mines

let set_is_num_visible (b : bool) (block : t) = block.is_num_visible <- b
let update_visibility (b : bool) (block : t) = set_is_visible b block
let update_mine (b : bool) (block : t) = set_has_mine b block
let update_flag (b : bool) (block : t) = set_has_flag b block

let update_num_surrounding_mines (i : int) (block : t) =
  set_num_surrounding_mines i block

let update_num_visible (b : bool) (block : t) = set_is_num_visible b block
let toggle_flag (block : t) = block.has_flag <- not block.has_flag

let reset_block (block : t) =
  block.has_mine <- false;
  block.is_visible <- false;
  block.has_flag <- false;
  block.num_surrounding_mines <- 0;
  block.is_num_visible <- false;
  block

let compare_blocks (block1 : t) (block2 : t) =
  block1.x = block2.x && block1.y = block2.y && block1.z = block2.z

let create (x', y', z', mine, vis) =
  let block =
    {
      x = x';
      y = y';
      z = z';
      has_mine = mine;
      is_visible = vis;
      has_flag = false;
      num_surrounding_mines = 0;
      is_num_visible = false;
    }
  in
  set_is_visible vis block;
  block

let is_flagged (block : t) = block.has_flag
let surrounding_mines (block : t) = block.num_surrounding_mines
let num_visible (block : t) = block.is_num_visible

let update_mine_visibility mine vis block =
  set_has_mine mine block;
  set_is_visible vis block;
  block

let toggle_mine_and_visibility block =
  block.has_mine <- not block.has_mine;
  block.is_visible <- not block.is_visible;
  block

let increment_surrounding_mines i block =
  set_num_surrounding_mines i block;
  block

let compare_blocks block1 block2 =
  block1.x = block2.x && block1.y = block2.y && block1.z = block2.z
