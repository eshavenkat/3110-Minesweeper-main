(** Block module implements the core functionality for Deep Minesweeper blocks.
    Each block represents a cell in the 3D grid with properties including
    coordinates, mine presence, visibility state, and depth-based coloring. *)

(* module type BlockSig = sig *)
type t
(** The abstract type representing a block in the game *)

val coordinates : t -> int * int * int
(** [coordinates b] returns the (x,y,z) coordinates of block [b] in the 3D grid.
    For example, [coordinates (create (1,2,3,false,true))] returns [(1,2,3)] *)

val has_mine : t -> bool
(** [mine b] returns whether block [b] contains a mine.
    @return [true] if the block contains a mine, [false] otherwise *)

val is_visible : t -> bool
(** [is_visible b] returns the visibility state of block [b].
    @return [true] if the block is visible, [false] if hidden *)

val has_flag : t -> bool
(** [has_flag b] returns whether block [b] has a flag.
    @return [true] if the block has a flag, [false] otherwise *)

val num_surrounding_mines : t -> int
(** [num_surrounding_mines b] returns the number of surrounding mines around
    block [b].
    @return An integer representing the number of mines *)

val is_num_visible : t -> bool
(** [is_num_visible b] returns whether the number of surrounding mines for block
    [b] is visible.
    @return [true] if the count is visible, [false] otherwise *)

val color : t -> int * int * int
(** [color b] returns the RGB color values for the block based on its
    z-coordinate:
    - z = 0: (51, 153, 51)
    - z = 1: (91, 119, 0)
    - z = 2: (106, 84, 0)
    - z = 3: (102, 51, 0)
    - z = 4: (84, 34, 16)
    - z = 5: (61, 23, 21)
    - z = 6: (35, 15, 17)
    - z > 6 or z < 0: (0, 0, 0)

    @return RGB color tuple (r,g,b) *)

val update_visibility : bool -> t -> unit
(** [update_visibility vis b] updates the visibility state of block b. For
    example, [update_visibility false b] creates a new hidden block *)

val update_mine : bool -> t -> unit
    (** [update_mine mine b] updates whether block b contains a mine.
    @param mine boolean value to set mine presence *)
    
val update_flag : bool -> t -> unit
(** [update_flag flag b] updates whether block b has a flag.
    @param flag boolean value to set flag presence *)
    
val update_num_visible : bool -> t -> unit
 (** [update_num_visible visible b] updates the visibility state of the surrounding mines number for block b.
    @param visible boolean value to set visibility of surrounding mines count *)
    
val update_num_surrounding_mines : int -> t -> unit
(** [update_num_surrounding_mines num b] updates the number of surrounding mines for block b.
    @param num integer value to increment surrounding mines *)

val create : int * int * int * bool * bool -> t
(** [create (x,y,z,mine,vis)] creates a new block with the specified properties:
    @param x X-coordinate in the grid
    @param y Y-coordinate in the grid
    @param z Z-coordinate (depth) in the grid
    @param mine Whether the block contains a mine
    @param vis Initial visibility state
    @return
      A new block with the specified properties.

      Example: [create (1,2,3,true,false)] creates a block at (1,2,3) with a
      mine that is initially hidden *)
val toggle_flag : t -> unit
(** [toggle_flag b] toggles the presence of a flag on block b. *)

val reset_block : t -> t
(** [reset_block b] resets the block to its initial state without mines, flags, or visible state. *)

val compare_blocks : t -> t -> bool
(** [compare_blocks b1 b2] compares two blocks to check if they are the same based on their coordinates.

    @return [true] if the blocks are the same, [false] otherwise *)

(* end module Block : BlockSig *)
(* Implementation of the Block signature *)
