open Core.Std

class board ~width ~height = object(self)
  val cells = Array.create ~len:(width * height) '.'

  method to_string = Array.to_list cells |> String.of_char_list

  method set_matching_points f =
    for y = 0 to height - 1 do
      for x = 0 to width - 1  do
        if f x y then self#draw_point x y
      done
    done

  method private draw_point x y =
    let index = y * width + x in
    Array.set cells index '*'
end

class virtual shape = object
  method virtual draw: board -> unit
end

class rectangle ~top_left:(x, y) ~width ~height = object
  inherit shape

  method draw (b: board) =
    b#set_matching_points (fun px py ->
      ((py = y || py = y + height - 1) && (x <= px && px < x + width)) ||
      ((px = x || px = x + width  - 1) && (y <= py && py < y + height))
    )
end

class circle ~centre:(x, y) ~radius = object
  inherit shape

  method draw (b: board) =
    b#set_matching_points (fun px py ->
      let d = (px - x) * (px - x) + (py - y) * (py - y) in
      d - radius * radius > (-2)  && d - radius * radius < 2
    )
end

let draw_shapes b shapes = List.iter shapes ~f:(fun s -> s#draw b)
