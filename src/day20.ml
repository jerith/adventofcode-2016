open Utils


type range = int * int

module Parser = struct
  open Angstrom

  let range =
    lift3 (fun a _ b -> a, b) P_misc.uinteger (char '-') P_misc.uinteger
  let ranges = sep_by end_of_line range
end


let range_to_str (a, b) = Printf.sprintf "%d-%d" a b

let max a b = if a < b then b else a

let combine ranges =
  let rec combine' combined = function
    | [] -> []
    | [r] -> List.rev (r :: combined)
    | (a0, b0) :: (a1, b1) :: ranges when a1 <= (b0+1) ->
      combine' combined ((a0, max b0 b1) :: ranges)
    | range :: ranges -> combine' (range :: combined) ranges
  in
  combine' [] ranges

let min_ip ranges =
  match List.hd ranges with
  | 0, n -> n+1
  | _ -> 0

let count_open top ranges =
  let rec count' sum bottom = function
    | [] -> sum + (top - bottom) + 1
    | (a, b) :: ranges -> count' (sum + (a - bottom)) (b+1) ranges
  in
  count' 0 0 ranges


let main_1 input =
  let ranges = List.sort compare input in
  noise_endline (map_to_str "\n" range_to_str ranges);
  let l = List.length ranges in
  let ranges = combine ranges in
  noisef "Reduced range count by %d.\n" (l - List.length ranges);
  noise_endline (map_to_str "\n" range_to_str ranges);
  min_ip ranges |> string_of_int


let main_2 input =
  let ranges = List.sort compare input in
  let ranges = combine ranges in
  noise_endline (map_to_str "\n" range_to_str ranges);
  count_open 4294967295 ranges |> string_of_int


type t = range list
let parser = Parser.ranges
