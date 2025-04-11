type t
(** Type representing the game board *)

exception MineClicked of string
(** Exception raised when a mine is clicked *)

exception WompWomp
(** Exception raised when a womp is encountered (non-mine with a flag) *)

val create : int * int * int -> t
(** Create a new game board *)

val rows : t -> int
(** Get the number of rows on the game board *)

val cols : t -> int
(** Get the number of columns on the game board *)

val height : t -> int
(** Get the height of the game board *)

val get : t -> int * int -> Block.t option
(** Get the block at a given position *)

val place_flag : t -> int * int -> unit
(** Place a flag at the given position *)

val poke : t -> int * int -> unit
(** Reveal the block at the given position *)

val dig : t -> int * int -> unit
(** Dig (reveal) the block at the given position *)

val defuse : t -> int * int -> unit
(** Defuse a mine at the given position *)

val get_z : t -> int * int -> int
(** Get the height index of the block at the given position *)

val add_mines : t -> int -> unit
(** Add mines to the game board *)

val count_mines : t -> int
(** Count the number of mines on the game board *)

val check_is_win : t -> bool
(** Check if the game is won *)

val serialize_game_state : t -> string
(** Serialization function to save the game state *)

val num_remaining_mines : t -> int -> int
(** Get the number of remaining mines on the game board *)
