open Utils


type block =
  | Literal of string
  | Marker of (string * int * int)

let block_to_str = function
  | Literal s -> s
  | Marker (s, _, _) -> s


module Parser = struct
  open Angstrom

  let text = take_while1 (function 'A' .. 'Z' -> true | _ -> false)
  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let a_by_b = lift3 (fun a _ b -> (a, b)) integer (string "x") integer
  let marker = char '(' *> a_by_b <* char ')' >>| (fun (a, b) ->
    Marker (Printf.sprintf "(%dx%d)" a b, a, b))
  let literal = text >>| (fun x -> Literal x)

  let data = many1 (literal <|> marker)
end


type lblock =
  | LLiteral of int
  | LMarker of (int * int * int)

let lblock_to_str = function
  | LLiteral l -> Printf.sprintf "[%d]" l
  | LMarker (l, a, b) -> Printf.sprintf "[%d(%dx%d)]" l a b


let block_to_lblock = function
  | Literal s -> LLiteral (String.length s)
  | Marker (s, a, b) -> LMarker (String.length s, a, b)

let lblock_size = function
  | LLiteral l -> l
  | LMarker (l, _, _) -> l

let split_lblock l1 = function
  | LLiteral l -> LLiteral l1, LLiteral (l-l1)
  | LMarker _ -> failwith "Can't split marker"

let rec process_lmarker pfun count l collected blocks =
  match l with
  | 0 -> pfun collected blocks count
  | n ->
    match blocks with
    | [] -> failwith "Unexpected end of input"
    | block :: blocks ->
      let newl = l - lblock_size block in
      match newl < 0 with
      | false -> process_lmarker pfun count newl (block::collected) blocks
      | true ->
        let b1, b2 = split_lblock l block in
        process_lmarker pfun count 0 (b1::collected) (b2::blocks)

let rec process_lblocks pfun size = function
  | [] -> size
  | LLiteral l :: blocks -> process_lblocks pfun (l+size) blocks
  | LMarker (_, l, c) :: blocks ->
    let decomp, blocks = process_lmarker pfun c l [] blocks in
    process_lblocks pfun (size + decomp) blocks

let countsize collected blocks count =
  let colsize = List.fold_left (fun s b -> s + lblock_size b) 0 collected in
  (count * colsize, blocks)

let rec subproc collected blocks count =
  let size = process_lblocks subproc 0 (List.rev collected) in
  (count * size, blocks)


let main_1 input =
  noise_endline @@ map_to_str "" block_to_str input;
  let lblocks = List.map block_to_lblock input in
  noise_endline @@ map_to_str "" lblock_to_str lblocks;
  process_lblocks countsize 0 lblocks |> string_of_int


let main_2 input =
  noise_endline @@ map_to_str "" block_to_str input;
  let lblocks = List.map block_to_lblock input in
  noise_endline @@ map_to_str "" lblock_to_str lblocks;
  process_lblocks subproc 0 lblocks |> string_of_int

type t = block list
let parser = Parser.data
