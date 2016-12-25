open Utils


type node = (int * int) * (int * int * int * int)


module Parser = struct
  open Angstrom

  let header = not_char '/' *> skip_many (not_char '\n') <* end_of_line
  let strpr str p = string str *> p
  let spaces = skip_many1 (char ' ')
  let tb = spaces *> P_misc.uinteger <* char 'T'
  let perc = spaces *> P_misc.uinteger <* char '%'

  let device = lift2 tuple2
      (strpr "/dev/grid/node-x" P_misc.uinteger) (strpr "-y" P_misc.uinteger)
  let stats = lift4 tuple4 tb tb tb perc
  let node = lift2 tuple2 device stats

  let nodes = many1 header *> sep_by end_of_line node
end


type storage = {
  nodes : (int * int) PairsMap.t;
  height : int;
  width : int;
  walls : PairsMap.key list;
}

let get st n = (n, PairsMap.find n st.nodes)

let node_to_str ((x, y), (s, u, a, p)) =
  Printf.sprintf
    "/dev/grid/node-x%d-y%d\t%4dT S\t%4dT U\t%4dT A\t(%d%%)" x y s u a p

let setup_nodes nodelist =
  let rec add' nodes walls = function
    | [] -> nodes, walls
    | (n, (c, u, a, _)) :: nodelist ->
      let nodes = PairsMap.add n (u, a) nodes in
      let walls = if c > 100 then n :: walls else walls in
      add' nodes walls nodelist
  in
  let nodes, walls = add' PairsMap.empty [] nodelist in
  let (mx, my), _ = PairsMap.max_binding nodes in
  {nodes; height = (my + 1); width = (mx + 1); walls}

let can_move (n0, (u0, _)) (n1, (_, a1)) = n0 <> n1 && u0 <> 0 && u0 <= a1

let viable_pairs st =
  let rec find' pairs = function
    | [] -> pairs
    | a :: rest ->
      let bs = List.filter (can_move a) (PairsMap.bindings st.nodes) in
      find' (List.rev_append (List.map (tuple2 a) bs) pairs) rest
  in
  find' [] (PairsMap.bindings st.nodes)

let adj_nodes st (x, y) =
  [x, y-1; x+1, y; x-1, y; x, y+1] |>
  List.filter (fun n -> PairsMap.mem n st.nodes) |>
  List.map (get st)

let move_data st (n0, (u0, a0)) (n1, (u1, a1)) =
  let nodes = PairsMap.add n1 (u1+u0, a1-u0) st.nodes in
  {st with nodes = PairsMap.add n0 (0, a0+u0) nodes}

let viable st n = List.exists (can_move (get st n)) (adj_nodes st n)
let target st n = List.exists (fun a -> can_move a (get st n)) (adj_nodes st n)

let print_nodes st data =
  let row y = Array.init st.width id |> Array.map (fun x -> x, y) in
  let n2vstr n =
    match n = data, List.mem n st.walls with
    | true, _ -> "d"
    | false, true -> "w"
    | _ ->
      match target st n, viable st n with
      | true, true -> "@" | true, false -> "#"
      | false, true -> "o" | false, false -> "."
  in
  let row2str y = map_to_str " " n2vstr (row y |> Array.to_list) in
  map_to_str "\n" row2str (Array.init st.height id |> Array.to_list) |>
  noise_endline

type move = {
  st : storage;
  steps : (PairsMap.key * PairsMap.key) list;
  data : PairsMap.key;
  target : PairsMap.key;
  score : int;
}

let get_targets st =
  let add_target n d ts = if target st n then (n :: ts) else ts in
  PairsMap.fold add_target st.nodes []

let get_viable st target =
  adj_nodes st target |> List.filter (fun n -> can_move n (get st target))

let wall_penalty st (tx, ty) =
  let rec checkwall' penalty = function
    | [] -> penalty
    | (wx, wy) :: _ when tx = wx && ty >= wy -> 200 + wx*2
    | (_, wy) :: walls when ty >= wy -> checkwall' (max penalty 100) walls
    | _ :: walls -> checkwall' penalty walls
  in checkwall' 0 st.walls

let calc_score st steps (dx, dy) (tx, ty) =
  List.fold_left (+) 0 [
    List.length steps;
    (dx + dy)*10;
    wall_penalty st (tx, ty);
    (abs (dx - tx) + abs (dy - ty))*2]

let make_move_for move target n =
  let st = move_data move.st n (get move.st target) in
  let steps = (target, fst n) :: move.steps in
  let data = if move.data = fst n then target else move.data in
  let target = fst n in
  let score = calc_score st steps data target in
  {st; steps; data; target; score}

let step_to_str ((x0, y0), (x1, y1)) =
  Printf.sprintf "%d,%d->%d,%d" x0 y0 x1 y1

let print_move move () =
  noisef "Score: %d Steps: %d, t = %d,%d\n"
    move.score (List.length move.steps) (fst move.target) (snd move.target);
  print_nodes move.st move.data


module Moves = struct
  type t = move
  let comptuple m = m.score, List.length m.steps, m.data, m.target
  let compare a b = compare (comptuple a) (comptuple b)
end
module MovesSet = Set.Make(Moves)

let pop_move moves =
  let move = MovesSet.min_elt moves in
  MovesSet.remove move moves, move

let add_move moves move = MovesSet.add move moves

let rec make_move moves =
  match pop_move moves with
  | moves, move when move.data = (0, 0) ->
    noisef "Done! "; ifnoise (print_move move);
    List.length move.steps
  | moves, move ->
    ifnoise (print_move move);
    let newmoves =
      get_viable move.st move.target |>
      List.map (make_move_for move move.target)
    in
    make_move @@ List.fold_left add_move moves newmoves

let first_move st target = {
  st;
  steps = [];
  data = (st.width - 1, 0);
  target = target;
  score = 0;
}

let first_moves st =
  get_targets st |> List.map (first_move st) |>
  List.fold_left add_move MovesSet.empty

let main_1 input =
  noise_endline @@ map_to_str "\n" node_to_str input;
  let st = setup_nodes input in
  let pairs = viable_pairs st in
  List.length pairs |> string_of_int


let main_2 input =
  let st = setup_nodes input in
  let moves = first_moves st in
  make_move moves |> string_of_int


type t = node list
let parser = Parser.nodes
