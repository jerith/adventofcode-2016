open Utils


module Parser = struct
  open Angstrom

  let number = P_misc.sinteger
end


let count_bits i =
  let rec count_bits' c = function
    | 0 -> c
    | i -> count_bits' (i mod 2 + c) (i lsr 1)
  in count_bits' 0 i


let is_wall number (x, y) =
  let v = x*x + 3*x + 2*x*y + y + y*y + number in
  count_bits v mod 2 = 1


let draw_maze number x y =
  let xs = Array.init x id |> Array.to_list in
  let draw_row y =
    xs |>
    List.map (fun x -> is_wall number (x, y)) |>
    List.map (function true -> "#" | false -> ".") |>
    String.concat "" |> (fun r -> Printf.printf "%2d %s\n" y r)
  in
  let tens x = if x mod 10 = 0 then string_of_int (x / 10) else " " in
  print_endline @@ String.concat "\n" [
    "   " ^ (map_to_str "" (fun x -> if x = 0 then " " else tens x) xs);
    "   " ^ (map_to_str "" (fun x -> x mod 10 |> string_of_int) xs)];
  Array.init y id |> Array.iter draw_row


let neighbours (x, y) =
  let nonneg (x, y) = x >= 0 && y >= 0 in
  List.filter nonneg [(x-1, y); (x+1, y); (x, y-1); (x, y+1)]


let next_steps number seen current =
    let can_go p = not (is_wall number p || (PairsSet.mem p seen)) in
    List.filter can_go (neighbours current)


let rec path_to number goal seen = function
  | [] -> failwith "Nowhere to go."
  | (s, pos) :: rest ->
    match pos = goal with
    | true -> s
    | false ->
      let next = List.map (fun p -> s+1, p) (next_steps number seen pos) in
      let seen = PairsSet.add pos seen in
      path_to number goal seen (List.merge IntPairs.compare rest next)


let rec find_locs number dist seen = function
  | [] -> PairsSet.cardinal seen
  | (s, pos) :: rest ->
    match s = dist with
    | true -> find_locs number dist (PairsSet.add pos seen) rest
    | false ->
      let next = List.map (fun p -> s+1, p) (next_steps number seen pos) in
      let seen = PairsSet.add pos seen in
      find_locs number dist seen (List.merge IntPairs.compare rest next)


let main_1 input =
  let vf = is_wall input in
  Printf.printf "number: %d v(1, 1): %b v(2, 2): %b\n"
    input (vf (1, 1)) (vf (2, 2));
  draw_maze input 20 20;
  let steps = path_to input (31, 39) PairsSet.empty [(0, (1, 1))] in
  Printf.printf "\nSteps to goal: %d\n" steps


let main_2 input =
  let vf = is_wall input in
  Printf.printf "number: %d v(1, 1): %b v(2, 2): %b\n"
    input (vf (1, 1)) (vf (2, 2));
  draw_maze input 20 20;
  let locs = find_locs input 50 PairsSet.empty [(0, (1, 1))] in
  Printf.printf "\nLocations reachable in %d steps: %d\n" 50 locs


type t = int
let parser = Parser.number
