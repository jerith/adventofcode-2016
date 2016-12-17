open Utils


module Parser = struct
  open Angstrom

  let code = P_misc.letters
end


type move = U | D | L | R

let move_to_str = function
  | U -> "U"
  | D -> "D"
  | L -> "L"
  | R -> "R"

module MoveList = struct
  type t = move list

  let move_to_int = function D -> 0 | R -> 1 | U -> 2 | L -> 3
  let compare_move m0 m1 = compare (move_to_int m0) (move_to_int m1)

  let rec compare_moves = function
    | [] -> 0
    | (m0, m1) :: moves when m0 = m1 -> compare_moves moves
    | (m0, m1) :: _ -> compare_move m0 m1

  let compare ml0 ml1 =
    match compare (List.length ml0) (List.length ml1) with
    | 0 -> compare_moves (List.combine ml0 ml1)
    | c -> c
end

module MovesSet = Set.Make(MoveList)


let move (x, y) = function
  | U -> (x, y-1)
  | D -> (x, y+1)
  | L -> (x-1, y)
  | R -> (x+1, y)

let walk path = List.fold_left move (0, 0) path

let path_str path = map_to_str "" move_to_str (List.rev path)

let hash_path code path =
  code ^ (path_str path) |> Digest.string |> Digest.to_hex

let door_open = function 'b' | 'c' | 'd' | 'e' | 'f' -> true | _ -> false

let room_valid (x, y) = x >= 0 && x < 4 && y >= 0 && y < 4

let move_valid room dir c = room_valid (move room dir) && door_open c

let queue_moves code paths room path =
  let queue_move paths (dir, c) =
    match move_valid room dir c with
    | true -> MovesSet.add (dir :: path) paths
    | false -> paths
  in
  let h = hash_path code path in
  List.fold_left queue_move paths [U, h.[0]; D, h.[1]; L, h.[2]; R, h.[3]]


let pop_min paths =
  let path = MovesSet.min_elt paths in
  MovesSet.remove path paths, path

let rec find_path code paths =
  let paths, path = pop_min paths in
  let room = walk path in
  noisef "%s -> (%d, %d)\n" (path_str path) (fst room) (snd room);
  match room with
  | (3, 3) -> paths, path
  | _ -> find_path code (queue_moves code paths room path)


let rec find_longest_path code (paths, path) =
  noise_endline (path_str path);
  try find_longest_path code (find_path code paths) with Not_found -> path


let main_1 input =
  let paths, path = find_path input (MovesSet.of_list [[]]) in
  path_str path


let main_2 input =
  let paths, path = find_path input (MovesSet.of_list [[]]) in
  let path = find_longest_path input (paths, path) in
  List.length path |> string_of_int

type t = string
let parser = Parser.code
