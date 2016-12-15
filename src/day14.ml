open Utils


module Parser = struct
  open Angstrom

  let salt = P_misc.letters
end


let rec find_triple = function
  | [] | [_] | [_; _] -> None
  | c0 :: c1 :: c2 :: tail ->
    match c0 = c1 && c1 = c2 with
    | true -> Some c0
    | false -> find_triple (c1 :: c2 :: tail)

let rec find_quintuple c str =
  let re = Str.regexp_string @@ String.make 5 c in
  try Str.search_forward re str 0 |> ignore; true
  with Not_found -> false

let mk_input salt i = salt ^ (string_of_int i)

let with_cache cache f input =
  match StringMap.mem input cache with
  | true -> cache, StringMap.find input cache
  | false ->
    let output = f input in
    StringMap.add input output cache, output

let rec check_for_quintuple hash cache c salt i = function
  | 0 -> cache, false
  | left ->
    let cache, md5 = with_cache cache hash (mk_input salt i) in
    match find_quintuple c md5 with
    | true -> cache, true
    | false -> check_for_quintuple hash cache c salt (i+1) (left-1)

let rec find_key hash cache salt i =
  let cache, md5 = with_cache cache hash (mk_input salt i) in
  (match i mod 500000 with
   | 0 -> Printf.printf "%s%d -> %s\n" salt i md5; flush_all ()
   | _ -> ());
  match find_triple (list_of_chars md5) with
  | None -> find_key hash cache salt (i+1)
  | Some c ->
    (* Printf.printf " ? %s%d -> %s\n" salt i md5; *)
    match check_for_quintuple hash cache c salt (i+1) 1000 with
    | cache, false -> find_key hash cache salt (i+1)
    | cache, true ->
      Printf.printf " * %s%d -> %s\n" salt i md5; flush_all ();
      cache, i, md5

let rec find_n_keys hash cache keys salt i = function
  | 0 -> cache, keys
  | n ->
    Printf.printf "%d:" n;
    let cache, i, key = find_key hash cache salt i in
    find_n_keys hash cache ((i, key) :: keys) salt (i+1) (n-1)


let hash input =
  Digest.string input |> Digest.to_hex

let rec times n f input =
  match n with
  | 0 -> input
  | _ -> times (n-1) f (f input)

let stretch_hash = times 2017 hash


let main_1 input =
  let cache, keys = find_n_keys hash StringMap.empty [] input 0 64 in
  let i, key = List.hd keys in
  Printf.printf "64th key for \"%s\": %d (%s)\n" input i key


let main_2 input =
  let cache, keys = find_n_keys stretch_hash StringMap.empty [] input 0 64 in
  let i, key = List.hd keys in
  Printf.printf "64th key for \"%s\": %d (%s)\n" input i key


type t = string
let parser = Parser.salt
