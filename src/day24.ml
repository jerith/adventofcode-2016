open Utils


type cell =
  | Wall
  | Clear
  | Start
  | Target of char

let cell_to_str = function
  | Wall -> "#"
  | Clear -> "."
  | Start -> "0"
  | Target t -> Char.escaped t


module Parser = struct
  open Angstrom

  let nzdigit = satisfy (function '1' .. '9' -> true | _ -> false)
  let cell = choice [
      char '#' *> return Wall;
      char '.' *> return Clear;
      char '0' *> return Start;
      nzdigit >>| fun d -> Target d]

  let row = many1 cell >>| Array.of_list
  let map = sep_by end_of_line row >>| Array.of_list
end

let row_to_str row = Array.to_list row |> map_to_str "" cell_to_str
let print_map map =
  Array.to_list map |> map_to_str "\n" row_to_str |> noise_endline


module Search = struct
  type t = {
    cost : int;
    cell : int * int;
    goal : int * int;
    path : (int * int) list;
  }

  let comptuple a = (a.cost, (- llen a.path), a.cell, a.path, a.goal)
  let compare a b = compare (comptuple a) (comptuple b)

  let to_str a =
    Printf.sprintf "Cost: %d Cell: %s Goal: %s Path: %s"
      a.cost (pairstr a.cell) (pairstr a.goal)
      (map_to_str "<-" pairstr a.path)
end

module SearchSet = Set.Make(Search)

let pop search =
  let step = SearchSet.min_elt search in
  SearchSet.remove step search, step

let push search step = SearchSet.add step search

let mdist (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)

let calc_cost cell goal path = llen path + mdist cell goal

let find_start_and_targets map =
  let check_cell y (start, targets) x =
    match map.(y).(x) with
    | Wall | Clear -> start, targets
    | Start -> (x, y), targets
    | Target _ -> start, (x, y) :: targets
  in
  let check_row things y =
    Array.fold_left (check_cell y) things (Array.init (alen map.(y)) id)
  in
  Array.fold_left check_row ((-1, -1), []) (Array.init (alen map) id)

let is_clear map (x, y) = match map.(y).(x) with Wall -> false | _ -> true

let adj_clear map (x, y) =
  List.filter (is_clear map) [x-1, y; x+1, y; x, y-1; x, y+1]

let move step cell = Search.(
    let path = step.cell :: step.path in
    let cost = calc_cost cell step.goal path in
    {step with cost; cell; path})

let rec find_path map visited search =
  let search, step = pop search in
  (* Search.to_str step |> noise_endline; *)
  match step.Search.cell = step.Search.goal with
  | true -> step.Search.path
  | false ->
    let search =
      adj_clear map step.Search.cell |>
      List.filter (fun c -> not (PairsSet.mem c visited)) |>
      List.map (move step) |> List.fold_left push search
    in
    find_path map (PairsSet.add step.Search.cell visited) search

let new_search cell goal =
  SearchSet.singleton Search.{cost = 0; cell; goal; path = []}

let find_path_length map start paths goal =
  let path = find_path map PairsSet.empty (new_search start goal) in
  noisef "Found path %s -> %s : %d\n"
    (pairstr start) (pairstr goal) (llen path);
  ((start, goal), llen path) :: paths

let find_path_lengths map goals paths start =
  List.fold_left (find_path_length map start) paths goals

let find_all_path_lengths map targets =
  List.fold_left (find_path_lengths map targets) [] targets

let permute goals =
  let rec permute' perm = function
    | [] -> [perm]
    | goals -> List.map (permute'' perm goals) goals |> List.concat
  and permute'' perm goals g =
    permute' (g :: perm) (List.filter ((<>) g) goals)
  in
  permute' [] goals

let rec path_length paths len start = function
  | [] -> len
  | target :: targets ->
    let len = len + List.assoc (start, target) paths in
    path_length paths len target targets

let find_path_length_through_targets map start targets =
  let paths = find_all_path_lengths map (start :: targets) in
  let pathlens =
    List.rev_map (path_length paths 0 start) (permute targets)
  in
  reduce min pathlens

let there_and_back_again map start targets =
  let paths = find_all_path_lengths map (start :: targets) in
  let perms = permute targets |> List.map (fun p -> List.rev (start :: p)) in
  let pathlens = List.rev_map (path_length paths 0 start) perms in
  reduce min pathlens


let main_1 input =
  print_map input;
  let start, targets = find_start_and_targets input in
  noisef "%d,%d %d\n" (fst start) (snd start) (llen targets);
  let pathlen = find_path_length_through_targets input start targets in
  string_of_int pathlen


let main_2 input =
  print_map input;
  let start, targets = find_start_and_targets input in
  noisef "%d,%d %d\n" (fst start) (snd start) (llen targets);
  let pathlen = there_and_back_again input start targets in
  string_of_int pathlen


type t = cell array array
let parser = Parser.map
