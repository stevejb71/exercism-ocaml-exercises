(** A board for drawing shapes on. *)
class board : width:int -> height:int -> object
  method to_string : string

  (** For each point on the board, passes x and y to the given
  predicate, and sets the points if the result is true *)
  method set_matching_points: (int -> int -> bool) -> unit
end

(** Virtual base class for a shape. *)
class virtual shape : object
  (** Draws this shape on the provided board *)
  method virtual draw: board -> unit
end

(** A rectangle with the top left at the given location and with given
width and height *)
class rectangle : top_left:(int * int) -> width:int -> height:int -> object
  inherit shape

  method draw : board -> unit
end

(** A circle with the centre at the given location and with given radius *)
class circle : centre:(int * int) -> radius:int -> object
  inherit shape

  method draw : board -> unit
end

(** Draws a list of shapes on the provided board *)
val draw_shapes : board -> shape list -> unit
