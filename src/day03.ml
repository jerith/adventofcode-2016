open Utils

module Parser = struct
  open Angstrom

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
  let side = skip_many (char ' ') *> integer
  let tri = lift3 (fun a b c -> (a, b, c)) side side side

  let sep = skip_many (char '\n')
  let triangles = sep_by sep tri
end


let tri_to_str (a, b, c) =
  String.concat "" [
    "("; string_of_int a; " "; string_of_int b; " "; string_of_int c; ")"]

let check_tri (a, b, c) =
  (a + b > c) && (a + c > b) && (b + c > a)


let main_1 input =
  let add_match c tri = c + match check_tri tri with true -> 1 | false -> 0 in
  let matches = List.fold_left add_match 0 input in
  print_endline @@ String.concat " " [
    "candidates:"; List.length input |> string_of_int;
    "triangles:"; string_of_int matches]


let rec horiz_to_vert collected = function
  | [] -> collected
  | (a, d, g) :: (b, e, h) :: (c, f, i) :: rest ->
    horiz_to_vert ((a, b, c) :: (d, e, f) :: (g, h, i) :: collected) rest
  | _ -> failwith "Columns didn't match up."

let main_2 input =
  main_1 @@ horiz_to_vert [] input


type t = (int * int * int) list
let parser = Parser.triangles
