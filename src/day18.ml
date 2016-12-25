open Utils


module Parser = struct
  open Angstrom

  let trap = char '^' *> return true
  let clear = char '.' *> return false
  let tile = trap <|> clear
  let tiles = many1 tile
end


let print_tiles tiles =
  noise_endline @@ map_to_str "" (function true -> "^" | false -> ".") tiles

let is_trap l c r = l <> r

let next_row tiles =
  let rec nr row = function
    | [] | [_] -> failwith "empty"
    | l :: c :: [] -> List.rev ((is_trap l c false) :: row)
    | l :: c :: r :: tiles -> nr ((is_trap l c r) :: row) (c :: r :: tiles)
  in
  nr [] (false :: tiles)

let count_row_clear row = List.fold_left (fun s t -> if t then s else s+1) 0 row

let rec count_clear clear row = function
  | 0 -> clear
  | n ->
    print_tiles row;
    count_clear (count_row_clear row + clear) (next_row row) (n-1)


let main_1 input =
  count_clear 0 input 40 |> string_of_int


let main_2 input =
  count_clear 0 input 400000 |> string_of_int

type t = bool list
let parser = Parser.tiles
