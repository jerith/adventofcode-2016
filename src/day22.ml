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
}

let get st n = (n, PairsMap.find n st.nodes)
let pos (n, _) = n

let node_to_str ((x, y), (s, u, a, p)) =
  Printf.sprintf
    "/dev/grid/node-x%d-y%d\t%4dT S\t%4dT U\t%4dT A\t(%d%%)" x y s u a p

let setup_nodes nodelist =
  let rec add' nodes = function
    | [] -> nodes
    | (n, (_, u, a, _)) :: nodelist ->
      add' (PairsMap.add n (u, a) nodes) nodelist
  in
  let nodes = add' PairsMap.empty nodelist in
  let (mx, my), _ = PairsMap.max_binding nodes in
  {nodes; height = (my + 1); width = (mx + 1)}

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
  [x-1, y; x+1, y; x, y-1; x, y+1] |>
  List.filter (fun (x, y) -> PairsMap.mem (x, y) st.nodes) |>
  List.map (get st)

let move_data st (n0, (u0, a0)) (n1, (u1, a1)) =
  let nodes = PairsMap.add n1 (u1+u0, a1-u0) st.nodes in
  {st with nodes = PairsMap.add n0 (0, a0+u0) nodes}

let viable st n = List.exists (can_move (get st n)) (adj_nodes st n)
let target st n = List.exists (fun a -> can_move a (get st n)) (adj_nodes st n)

let print_nodes st =
  let row y = Array.init st.width id |> Array.map (fun x -> x, y) in
  (* let mapnode f n = let u, a = PairsMap.find n nodes in f u a in *)
  (* let n2str u a = Printf.sprintf "%3d:%-2d" u a in *)
  (* let n2cstr u a = Printf.sprintf "  %3d " (u+a) in *)
  let n2vstr n =
    match target st n, viable st n with
    | true, true -> " @" | true, false -> " #"
    | false, true -> " o" | false, false -> " ."
  in
  let row2str y =
    String.concat "\n" [
      (* map_to_str " " (mapnode n2str) (row y |> Array.to_list); *)
      (* map_to_str " " (mapnode n2cstr) (row y |> Array.to_list); *)
      map_to_str " " n2vstr (row y |> Array.to_list);
    ]
  in
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

let get_viable st t =
  adj_nodes st t |> List.filter (fun n -> can_move n (get st t))

let calc_score st steps (dx, dy) (tx, ty) =
  List.length steps + dx + dy + abs (dx - tx) + abs (dy - ty)

let make_move_for move target n =
  let st = move_data move.st n (get move.st target) in
  let steps = (target, pos n) :: move.steps in
  let score = calc_score st steps move.data target in
  {move with st; steps; target; score}

let make_moves_to move t =
  let viables = get_viable move.st t in
  List.map (make_move_for move t) viables

let step_to_str ((x0, y0), (x1, y1)) =
  Printf.sprintf "%d,%d->%d,%d" x0 y0 x1 y1

let print_steps steps = map_to_str " " step_to_str steps |> noise_endline

let rec make_move = function
  (* | [] -> failwith "Failed. :-(" *)
  | ml when List.length ml > 100 -> failwith "Too many."
  | [] -> 0
  | move :: moves ->
    noisef "Steps: %d, target= %d,%d\n" (List.length move.steps) (fst move.target) (snd move.target);
    print_steps move.steps;
    print_nodes move.st;
    if move.target = (0, 0) then failwith "foo" else ();
    let targets = get_targets move.st in
    let movelists = List.map (make_moves_to move) targets in
    let moves =
      List.concat (moves :: movelists) |>
      List.sort (fun a b -> compare a.score b.score)
    in
    make_move moves

let first_move st = {
  st;
  steps = [];
  data = (0, st.width - 1);
  target = (0, st.width - 1);
  score = 0;
}


let main_1 input =
  noise_endline @@ map_to_str "\n" node_to_str input;
  let st = setup_nodes input in
  let pairs = viable_pairs st in
  List.length pairs |> string_of_int


let main_2 input =
  let st = setup_nodes input in
  print_nodes st;
  make_move [(first_move st)] |> string_of_int


type t = node list
let parser = Parser.nodes
