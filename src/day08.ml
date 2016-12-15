open Utils


type instruction =
  | Rect of (int * int)
  | RotCol of (int * int)
  | RotRow of (int * int)

let inst_to_str = function
  | Rect (x, y) -> Printf.sprintf "rect %dx%d" x y
  | RotCol (c, a) -> Printf.sprintf "rot col %d by %d" c a
  | RotRow (r, a) -> Printf.sprintf "rot row %d by %d" r a

module Parser = struct
  open Angstrom

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let rect =
    lift4 (fun _ x _ y -> Rect (x, y))
      (string "rect ") integer (char 'x') integer

  let a_by_b = lift3 (fun a _ b -> (a, b)) integer (string " by ") integer

  let rot_col = string "rotate column x=" *> a_by_b >>| fun ab -> RotCol ab
  let rot_row = string "rotate row y=" *> a_by_b >>| fun ab -> RotRow ab

  let instruction = rect <|> rot_col <|> rot_row

  let instructions = sep_by end_of_line instruction
end


let mkscreen x y = Array.make_matrix y x false


let screen_to_str' s c screen =
  let pixel_to_str = function true -> s | false -> c in
  let row_to_str row =
    Array.map pixel_to_str row |> Array.to_list |> String.concat ""
  in
  Array.map row_to_str screen |> Array.to_list |> String.concat "\n"

let screen_to_str = screen_to_str' "#" "."

let set_rect (x, y) screen =
  for r = 0 to y - 1 do
    Array.fill screen.(r) 0 x true
  done;
  screen

let rot_col (c, a) screen =
  let h = Array.length screen in
  let col = Array.init h (fun i -> screen.(i).(c)) in
  for r = 0 to h-1 do
    screen.((r+a) mod h).(c) <- col.(r)
  done;
  screen

let rot_row (r, a) screen =
  let row = Array.copy screen.(r) in
  Array.blit row 0 screen.(r) a (Array.length row - a);
  Array.blit row (Array.length row - a) screen.(r) 0 a;
  screen

let update_screen screen = function
  | Rect d -> set_rect d screen
  | RotCol d -> rot_col d screen
  | RotRow d -> rot_row d screen

let count_pixels screen =
  let count_set s p = s + (if p then 1 else 0) in
  let count_row s row = s + Array.fold_left count_set 0 row in
  Array.fold_left count_row 0 screen

let main_1 input =
  List.fold_left update_screen (mkscreen 50 6) input |>
  count_pixels |> string_of_int

let main_2 input =
  List.fold_left update_screen (mkscreen 50 6) input |>
  screen_to_str' "#" " " |> noise_endline;
  (* TODO: Turn this into a string. *)
  ""

type t = instruction list
let parser = Parser.instructions
