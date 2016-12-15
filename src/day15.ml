open Utils


type disc =
  | Disc of (int * int * int)

let disc_to_str = function
  | Disc (n, p, s) ->
    Printf.sprintf "Disc #%d: %d positions, starts at %d." n p s


module Parser = struct
  open Angstrom

  let discnum = string "Disc #" *> P_misc.uinteger
  let positions = string " has " *> P_misc.uinteger <* string " positions;"
  let start = string " at time=0, it is at position " *> P_misc.uinteger
  let disc =
    lift3 (fun n p s -> Disc (n, p, s)) discnum positions start <* char '.'

  let discs = sep_by end_of_line disc
end


let has_gap t (Disc (n, p, s)) = (n + s + t) mod p = 0

let all_gaps t discs = List.for_all (has_gap t) discs

let rec first_clear_time t discs =
  match all_gaps t discs with
  | true -> t
  | false -> first_clear_time (t+1) discs


let main_1 input =
  noise_endline @@ map_to_str "\n" disc_to_str input;
  first_clear_time 0 input |> string_of_int


let main_2 input =
  let discs = Disc (List.length input + 1, 11, 0) :: input in
  noise_endline @@ map_to_str "\n" disc_to_str discs;
  first_clear_time 0 discs |> string_of_int


type t = disc list
let parser = Parser.discs
