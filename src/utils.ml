let parse_input_file a_parser filename =
  let module AB = Angstrom.Buffered in
  let pstate = AB.parse a_parser in
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

let noise str =
  match !quiet with
  | true -> ()
  | false -> print_string str; flush stdout

let noise_endline str = noise @@ str ^ "\n"

let noisef fmt = Printf.ksprintf noise fmt


let map_to_str sep f l =
  String.concat sep @@ List.map f l


let list_of_chars string =
  let rec to_list chars = function
    | "" -> List.rev chars
    | s -> to_list (s.[0] :: chars) (Str.string_after s 1)
  in
  to_list [] string


let id x = x


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
