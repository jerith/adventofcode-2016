open Utils

type dir =
  | Up
  | Down
  | Left
  | Right

let dir_to_str = function
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"

let dir_to_str_compact dir = String.sub (dir_to_str dir) 0 1

module Parser = struct
  open Angstrom

  let up = char 'U' *> return Up
  let down = char 'D' *> return Down
  let left = char 'L' *> return Left
  let right = char 'R' *> return Right

  let dir = choice [up; down; left; right]
  let seq = many1 dir

  let sep = skip_many (char '\n')

  let instructions = sep_by sep seq

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
    let doc = "part 1 or part 1" in
    Arg.(required & pos 1 (some int) None & info [] ~docv:"PART" ~doc)

  let run_main mainfunc =
    let main_t = Term.(const mainfunc $ input_filename $ part) in
    let info = Term.info "day01" ~doc:"day 1 solver" in
    match Term.eval (main_t, info) with `Error _ -> exit 1 | _ -> exit 0
end


let seq_to_str seq =
  String.concat " " @@ List.map dir_to_str seq

let but_to_str (x, y) =
  String.concat "" ["("; string_of_int x; ", "; string_of_int y; ")"]

let add_key pad (coords, value) =
  PairsMap.add coords value pad

let get_button pad key =
  PairsMap.find key pad

let next_button pad (x, y) dir =
  let next = match dir with
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)
  in match PairsMap.mem next pad with
  | true -> next
  | false -> (x, y)

let rec follow_seq pad button = function
  | [] -> button
  | dir :: seq ->
    follow_seq pad (next_button pad button dir) seq

let rec follow_seqs pad button buttons = function
  | [] -> List.rev buttons
  | seq :: seqs ->
    let next_button = follow_seq pad button seq in
    (* print_endline @@ String.concat "" [ *)
    (*   (get_button pad button); " -> "; *)
    (*   String.concat "" @@ List.map dir_to_str_compact seq; *)
    (*   " -> "; (get_button pad next_button)]; *)
    follow_seqs pad next_button (next_button :: buttons) seqs

let get_start pad =
  let only_five = PairsMap.filter (fun _ x -> String.equal x "5") pad in
  let button, _ = PairsMap.choose only_five in
  button

let text_to_keypad lines =
  let rec add_line pad y i line =
    let len = String.length line in
    match len - i with
    | 0 -> pad
    | n ->
      match line.[i] with
      | ' ' -> add_line pad y (i+1) line
      | c -> add_line (add_key pad ((i, y), (String.make 1 c))) y (i+1) line
  in
  let rec add_lines pad y = function
    | [] -> pad
    | line :: lines ->
      add_lines (add_line pad y 0 line) (y+1) lines
  in
  add_lines PairsMap.empty 0 lines


let get_keypad_1 () =
  text_to_keypad [
    "123";
    "456";
    "789"]

let main_1 input =
  let pad = get_keypad_1 () in
  let start = get_start pad in
  print_endline @@ map_to_str "" (get_button pad)
    (follow_seqs pad start [] input)

let get_keypad_2 () =
  text_to_keypad [
    "  1  ";
    " 234 ";
    "56789";
    " ABC ";
    "  D  "]

let main_2 input =
  let pad = get_keypad_2 () in
  let start = get_start pad in
  print_endline @@ map_to_str "" (get_button pad)
    (follow_seqs pad start [] input)


let main filename part =
  let input = parse_input_file Parser.instructions filename in
  match part with
  | 1 -> main_1 input
  | 2 -> main_2 input
  | n -> failwith ("Unknown part: " ^ (string_of_int n))

let () = Args.run_main main
