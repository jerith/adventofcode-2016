open Utils

module Parser = struct
  open Angstrom

  let checksum = char '[' *> take 5 <* char ']'
  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
  let zone = char '-' *> integer
  let word =
    take_while1 (function 'a' .. 'z' -> true | _ -> false)
  let name = sep_by (char '-') word >>| String.concat "-"

  let room = lift3 (fun n z c -> (n, z, c)) name zone checksum

  let rooms = sep_by (skip_many1 end_of_line) room
end


let room_to_str (name, zone, checksum) =
  name ^ "::" ^ (string_of_int zone) ^ " [" ^ checksum ^ "]"


let init_cm freqs i =
  CharMap.add (Char.chr @@ i + Char.code 'a') 0 freqs

let update_cm freqs c =
  let inc_cm k v =
    match c == k with
    | true -> v + 1
    | false -> v
  in
  CharMap.mapi inc_cm freqs

let count_frequencies str =
  let letters = Array.init 26 (fun a -> a) in
  let freqs = Array.fold_left init_cm CharMap.empty letters in
  let chars = Array.init (String.length str) (fun i -> str.[i]) in
  Array.fold_left update_cm freqs chars

let sort_freqlist freqlist =
  let compare (f0, c0) (f1, c1) =
    match Pervasives.compare f1 f0 with
    | 0 -> Pervasives.compare c0 c1
    | c -> c
  in
  List.sort compare freqlist

let compute_checksum name =
  let freqs = count_frequencies name in
  let freqlist = List.map (fun (a, b) -> (b, a)) (CharMap.bindings freqs) in
  let sortedstr =
    sort_freqlist freqlist |>
    List.map (fun (_, c) -> Char.escaped c) |>
    String.concat ""
  in
  String.sub sortedstr 0 5


let add_valid_rooms sum (name, zone, checksum) =
  match compute_checksum name |> String.equal checksum with
  | true -> sum + zone
  | false -> sum


let shift_char num = function
  | 'a' .. 'z' as c ->
    let i = Char.code c - Char.code 'a' in
    let si = (i + num) mod 26 in
    Char.chr @@ si + Char.code 'a'
  | c -> c


let decipher (name, zone, checksum) =
  String.map (shift_char zone) name


let print_room (name, zone, checksum as room) =
  noise_endline @@ String.concat " " [
    decipher room;
    "     ";
    room_to_str room;
    compute_checksum name;
  ]

let main_1 input =
  input |>
  List.filter (fun (n, z, c) -> String.equal c (compute_checksum n)) |>
  List.fold_left (fun sum (n, z, c) -> sum + z) 0 |>
  string_of_int


let filter_north room =
  let plaintext = decipher room in
  let re = Str.regexp_string "north" in
  try ignore (Str.search_forward re plaintext 0); true with _ -> false

let main_2 input =
  (* List.iter print_room input; *)
  let filtered = List.filter filter_north input in
  filtered |>
  map_to_str "\n" (fun room ->
      String.concat " " [decipher room; room_to_str room]) |>
  noise_endline;
  let _, zone, _ = List.hd filtered in
  string_of_int zone


type t = (string * int * string) list
let parser = Parser.rooms
