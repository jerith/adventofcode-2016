open Utils


type operation =
  | SwapPos of int * int
  | SwapChar of char * char
  | RotateLeft of int
  | RotateRight of int
  | RotatePos of char
  | Reverse of int * int
  | Move of int * int

let op_to_str = function
  | SwapPos (a, b) -> Printf.sprintf "swap position %d with %d" a b
  | SwapChar (a, b) -> Printf.sprintf "swap letter %c with %c" a b
  | RotateLeft a -> Printf.sprintf "rotate left by %d" a
  | RotateRight a -> Printf.sprintf "rotate right by %d" a
  | RotatePos a -> Printf.sprintf "rotate based on position of %c" a
  | Reverse (a, b) -> Printf.sprintf "reverse positions %d through %d" a b
  | Move (a, b) -> Printf.sprintf "move position %d to %d" a b


module Parser = struct
  open Angstrom

  let pos = P_misc.uinteger
  let ch = satisfy (function 'a' .. 'z' -> true | _ -> false)
  let steps = P_misc.uinteger <* string " step" <* option 's' (char 's')
  let strpr s p = string s *> p

  let swappos = lift2 (fun a b -> SwapPos (a, b))
      (strpr "swap position " pos) (strpr " with position " pos)
  let swapchar = lift2 (fun a b -> SwapChar (a, b))
      (strpr "swap letter " ch) (strpr " with letter " ch)
  let rotleft = lift (fun a -> RotateLeft a) (strpr "rotate left " steps)
  let rotright = lift (fun a -> RotateRight a) (strpr "rotate right " steps)
  let rotpos = lift (fun a -> RotatePos a)
      (strpr "rotate based on position of letter " ch)
  let reverse = lift2 (fun a b -> Reverse (a, b))
      (strpr "reverse positions " pos) (strpr " through " pos)
  let move = lift2 (fun a b -> Move (a, b))
      (strpr "move position " pos) (strpr " to position " pos)

  let op = choice [swappos; swapchar; rotleft; rotright; rotpos; reverse; move]
  let ops = sep_by end_of_line op
end


let swappos (a, b) passwd =
  String.mapi (fun i c -> match i with
      | i when i = a -> passwd.[b]
      | i when i = b -> passwd.[a]
      | _ -> c
    ) passwd

let swapchar (a, b) passwd=
  String.map (function
      | c when c = a -> b
      | c when c = b -> a
      | c -> c
    ) passwd

let rotleft a passwd =
  let l = String.length passwd in
  let a = a mod l in
  (String.sub passwd a (l-a)) ^ (String.sub passwd 0 a)

let rotright a passwd =
  let l = String.length passwd in
  let a = a mod l in
  (String.sub passwd (l-a) a) ^ (String.sub passwd 0 (l-a))

let rotpos a passwd =
  let i = String.index passwd a in
  let i = if i >= 4 then i+2 else i+1 in
  rotright i passwd

let reverse (a, b) passwd =
  String.mapi (fun i c -> match i with
      | i when i < a || i > b -> c
      | i -> passwd.[b-(i-a)]
    ) passwd

let move (a, b) passwd =
  let l = String.length passwd in
  match a < b with
  | true -> String.concat "" [
    if a = 0 then "" else String.sub passwd 0 a;
    rotleft 1 (String.sub passwd a (b-a+1));
    if b = l-1 then "" else String.sub passwd (b+1) (l-b-1)]
  | false -> String.concat "" [
    if b = 0 then "" else String.sub passwd 0 b;
    rotright 1 (String.sub passwd b (a-b+1));
    if a = l-1 then "" else String.sub passwd (a+1) (l-a-1)]

let perform_op passwd op =
  noisef "%s -> %s\n" passwd (op_to_str op);
  match op with
  | SwapPos (a, b) -> swappos (a, b) passwd
  | SwapChar (a, b) -> swapchar (a, b) passwd
  | RotateLeft a -> rotleft a passwd
  | RotateRight a -> rotright a passwd
  | RotatePos a -> rotpos a passwd
  | Reverse (a, b) -> reverse (a, b) passwd
  | Move (a, b) -> move (a, b) passwd

let rotpos_rev a passwd =
  let rec find asswdp =
    match rotpos a asswdp = passwd with
    | true -> asswdp
    | false -> find (rotleft 1 asswdp)
  in
  find passwd

let perform_op_rev passwd op =
  noisef "%s -> %s\n" passwd (op_to_str op);
  match op with
  | SwapPos (a, b) -> swappos (a, b) passwd
  | SwapChar (a, b) -> swapchar (a, b) passwd
  | RotateLeft a -> rotright a passwd
  | RotateRight a -> rotleft a passwd
  | RotatePos a -> rotpos_rev a passwd
  | Reverse (a, b) -> reverse (a, b) passwd
  | Move (a, b) -> move (b, a) passwd

let rec process perform passwd ops =
  List.fold_left perform passwd ops


let main_1 input =
  noise_endline @@ map_to_str "\n" op_to_str input;
  let passwd = "abcdefgh" in
  process perform_op passwd input


let main_2 input =
  noise_endline @@ map_to_str "\n" op_to_str input;
  let passwd = "fbgdceah" in
  process perform_op_rev passwd (List.rev input)


type t = operation list
let parser = Parser.ops
