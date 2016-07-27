open Core.Std

type board = {width: int; cells: char array}

let new_board width height = {width; cells = Array.create (width * height) ' '}

let draw_point {width; cells} x y =
  let index = y * width + x in
  Array.set cells index '*'

class rectangle w h = object
  method draw (b: board) = ()
end

class circle r = object
  method draw (b: board) = ()
end
