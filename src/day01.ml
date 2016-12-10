open Utils

type turn_dir =
  | Left
  | Right

type movement = turn_dir * int

module Parser = struct
  open Angstrom

  let turn_left = char 'L' *> return Left
  let turn_right = char 'R' *> return Right

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let instruction =
    (turn_left <|> turn_right) >>= (fun d ->
        integer >>| fun i -> (d, i))

  let sep = char ',' *> skip_many (char ' ')

  let instructions = sep_by sep instruction

  let parse_input inputstr =
    match parse_only instructions (`String inputstr) with
    | Result.Ok v -> v
    | Result.Error msg -> failwith msg
end


module Args = struct
  open Cmdliner

  let input_filename =
    let doc = "file containing input data" in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

  let part =
    let doc = "part 1 or part 2" in
    Arg.(required & pos 1 (some int) None & info [] ~docv:"PART" ~doc)

  let run_main mainfunc =
    let main_t = Term.(const mainfunc $ input_filename $ part) in
    let info = Term.info "day01" ~doc:"day 1 solver" in
    match Term.eval (main_t, info) with `Error _ -> exit 1 | _ -> exit 0
end

type dir = N | E | S | W

let loc_to_str (n, e, d) =
  String.concat " " [
    "N: " ^ (string_of_int n);
    "E: " ^ (string_of_int e);
    "D: " ^ (match d with N -> "N" | E -> "E" | S -> "S" | W -> "W");
    "Total: " ^ (string_of_int @@ (abs n) + (abs e))]

let turn dir turn_dir =
  match (dir, turn_dir) with
  | N, Left | S, Right -> W
  | E, Left | W, Right -> N
  | S, Left | N, Right -> E
  | W, Left | E, Right -> S

let walk_block (n, e, d) distance = match d with
  | N -> (n + distance, e, N)
  | E -> (n, e + distance, E)
  | S -> (n - distance, e, S)
  | W -> (n, e - distance, W)

let apply_instruction (n, e, d) (turn_dir, distance) =
  walk_block (n, e, (turn d turn_dir)) distance


let main_1 walk =
  let loc = List.fold_left apply_instruction (0, 0, N) walk in
  print_endline @@ loc_to_str loc


let rec walk_and_check visited (n, e, d) blocks =
  match blocks with
  | 0 -> (false, visited, (n, e, d))
  | _ ->
    let (n, e, d) = walk_block (n, e, d) 1 in
    match PairsSet.mem (n, e) visited with
    | true ->
      (true, visited, (n, e, d))
    | false ->
      walk_and_check (PairsSet.add (n, e) visited) (n, e, d) (blocks - 1)

let rec find_first_repeat visited (n, e, d) walk =
  match walk with
  | [] -> failwith "No location visited twice!"
  | (turn_dir, distance)::tl ->
    let d = turn d turn_dir in
    match walk_and_check visited (n, e, d) distance with
    | true, visited, location -> location
    | false, visited, location ->
      find_first_repeat visited location tl

let main_2 walk =
  let loc = find_first_repeat (PairsSet.of_list [(0, 0)]) (0, 0, N) walk in
  print_endline @@ loc_to_str loc


let main filename part =
  let input = parse_input_file Parser.instructions filename in
  match part with
  | 1 -> main_1 input
  | 2 -> main_2 input
  | n -> failwith ("Unknown part: " ^ (string_of_int n))

let () = Args.run_main main
