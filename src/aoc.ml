module Args = struct
  open Cmdliner

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
    let main_t = Term.(const mainfunc $ day $ input_filename $ part) in
    let info = Term.info "aoc" ~doc:"Advent of Code solver" in
    match Term.eval (main_t, info) with `Error _ -> exit 1 | _ -> exit 0
end


module type Day = sig
  type t
  val main_1 : t -> unit
  val main_2 : t -> unit
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
]

let get_day n =
  match n > 0 && n <= List.length days with
  | true -> List.nth days (n-1)
  | false -> failwith @@ "Invalid day: " ^ (string_of_int n)


let call_solver (module D : Day) filename part =
  let input = Utils.parse_input_file D.parser filename in
  match part with
  | 1 -> D.main_1 input
  | 2 -> D.main_2 input
  | n -> failwith ("Unknown part: " ^ (string_of_int n))


let main (type a) day filename part =
  call_solver (get_day day) filename part

let () = Args.run_main main
