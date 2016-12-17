module Args = struct
  open Cmdliner

  let quiet =
    let doc = "suppress the noise" in
    Arg.(value & flag & info ["q"; "quiet"] ~doc)

  let answers =
    let doc = "check answers against those in file" in
    Arg.(value & opt (some file) None &
         info ["a"; "answers"] ~docv:"FILENAME" ~doc)

  let day =
    let doc = "day (1 through 25)" in
    Arg.(required & pos 0 (some int) None & info [] ~docv:"DAY" ~doc)

  let input_filename =
    let doc = "file containing input data" in
    Arg.(required & pos 1 (some file) None & info [] ~docv:"FILENAME" ~doc)

  let part =
    let doc = "part (1 or 2)" in
    Arg.(required & pos 2 (some int) None & info [] ~docv:"PART" ~doc)

  let run_main mainfunc =
    let main_t =
      Term.(const mainfunc $ quiet $ answers $ day $ input_filename $ part) in
    let info = Term.info "aoc" ~doc:"Advent of Code solver" in
    match Term.eval (main_t, info) with `Error _ -> exit 1 | _ -> exit 0
end


module type Day = sig
  type t
  val main_1 : t -> string
  val main_2 : t -> string
  val parser : t Angstrom.t
end


let days = [
  (module Day01 : Day);
  (module Day02 : Day);
  (module Day03 : Day);
  (module Day04 : Day);
  (module Day05 : Day);
  (module Day06 : Day);
  (module Day07 : Day);
  (module Day08 : Day);
  (module Day09 : Day);
  (module Day10 : Day);
  (module Day11 : Day);
  (module Day12 : Day);
  (module Day13 : Day);
  (module Day14 : Day);
  (module Day15 : Day);
  (module Day16 : Day);
  (module Day17 : Day);
]

let get_day n =
  match n > 0 && n <= List.length days with
  | true -> List.nth days (n-1)
  | false -> failwith @@ "Invalid day: " ^ (string_of_int n)


let get_answer answers_file part =
  let inch = open_in answers_file in
  let rec read_answer last = function
    | 0 -> last
    | n -> read_answer (input_line inch) (n-1)
  in
  read_answer "" part


let call_solver d (module D : Day) input_file part answers_file =
  let input = Utils.parse_input_file D.parser input_file in
  let answer = match part with
    | 1 -> D.main_1 input
    | 2 -> D.main_2 input
    | n -> failwith ("Unknown part: " ^ (string_of_int n))
  in
  Printf.printf "Answer for %d.%d: %s\n" d part answer;
  match answers_file with
  | None -> ()
  | Some answers_file ->
    let expected = get_answer answers_file part in
    match answer = expected with
    | true -> print_endline "Answers match."
    | false -> failwith @@ "Expected '" ^ expected ^ "', got '" ^ answer ^ "'"

let main (type a) quiet answers_file day input_file part =
  Utils.quiet := quiet;
  call_solver day (get_day day) input_file part answers_file

let () = Args.run_main main
