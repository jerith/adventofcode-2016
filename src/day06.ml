open Utils

module Parser = struct
  open Angstrom

  let line = take_while1 (function '\r' | '\n' -> false | _ -> true)
  let sep = skip_many end_of_line
  let lines = sep_by sep line
end


let update_cm freqs c =
  let v = try CharMap.find c freqs with _ -> 0 in
  CharMap.add c (v+1) freqs

let count_frequencies strs pos =
  let update_cm' freqs str = update_cm freqs  str.[pos] in
  List.fold_left update_cm' CharMap.empty strs

let sort_freqlist freqlist =
  let compare (c0, f0) (c1, f1) =
    match Pervasives.compare f1 f0 with
    | 0 -> Pervasives.compare c0 c1
    | c -> c
  in
  List.sort compare freqlist

let decode_pos decoder strs pos =
  count_frequencies strs (pos-1) |> decoder

let rec decode decoder msg strs = function
  | 0 -> List.map Char.escaped msg |> String.concat ""
  | pos -> decode decoder (decode_pos decoder strs pos :: msg) strs (pos-1)

let max_freq freqs =
  CharMap.bindings freqs |> sort_freqlist |> List.hd |> fst


let main_1 input =
  decode max_freq [] input (String.length @@ List.hd input)


let min_freq freqs =
  CharMap.bindings freqs |> sort_freqlist |> List.rev |> List.hd |> fst

let main_2 input =
  decode min_freq [] input (String.length @@ List.hd input)


type t = string list
let parser = Parser.lines
