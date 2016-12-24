let wrap_parser a_parser =
  Angstrom.(a_parser <* skip_many end_of_line <* end_of_input)

let parse_input_file a_parser filename =
  let module AB = Angstrom.Buffered in
  let pstate = AB.parse (wrap_parser a_parser) in
  let inch = open_in_bin filename in
  let buf = Bytes.create 4096 in
  let rec read pst =
    match input inch buf 0 4096 with
    | 0 -> (match AB.state_to_result (AB.feed pst `Eof) with
        | Result.Ok v -> v
        | Result.Error msg -> failwith msg)
    | n -> read @@ AB.feed pst @@ `String (Bytes.sub_string buf 0 n)
  in read pstate


let quiet = ref false

let ifnoise f =
  match !quiet with
  | true -> ()
  | false -> f ()

let noise str = ifnoise (fun () -> print_string str; flush stdout)
let noise_endline str = noise @@ str ^ "\n"
let noisef fmt = Printf.ksprintf noise fmt


let map_to_str sep f l =
  String.concat sep @@ List.rev_map f (List.rev l)


let list_of_chars string =
  let rec to_list chars = function
    | 0 -> chars
    | i -> to_list (string.[i-1] :: chars) (i-1)
  in
  to_list [] (String.length string)

let id x = x

let llen = List.length
let slen = String.length
let alen = Array.length

let sum = List.fold_left (+) 0
let reduce f l = List.fold_left f (List.hd l) (List.tl l)

let pairstr (a, b) = Printf.sprintf "%d,%d" a b


module IntPairs = struct
  type t = int * int
  let compare (x0,y0) (x1,y1) =
    match compare x0 x1 with 0 -> compare y0 y1 | c -> c
end

module PairsSet = Set.Make(IntPairs)
module PairsMap = Map.Make(IntPairs)
module CharMap = Map.Make(Char)
module IntMap = Map.Make(struct type t = int let compare = compare end)
module StringMap = Map.Make(String)


module P_misc = struct
  open Angstrom

  let uinteger =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
  let sign = (char '-' *> return (-1)) <|> (return 1)
  let sinteger = lift2 (fun s i -> s * i) sign uinteger

  let letters =
    take_while1 (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)

end

let tuple2 a b = a, b
let tuple3 a b c = a, b, c
let tuple4 a b c d = a, b, c, d
