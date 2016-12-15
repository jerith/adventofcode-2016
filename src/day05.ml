open Utils

module Parser = struct
  open Angstrom

  let name =
    take_while1 (function
        | '0' .. '9' -> true
        | 'a' .. 'z' -> true
        | 'A' .. 'Z' -> true
        | '-' -> true
        | _ -> false)
end


let try_pass name i =
  let namei = name ^ (string_of_int i) in
  let md5 = Digest.string namei |> Digest.to_hex in
  (match i mod 500000 with
   | 0 -> noise_endline @@ namei ^ " -> " ^ md5
   | _ -> ());
  match String.sub md5 0 5 with
  | "00000" ->
    (noise_endline @@ " * " ^ namei ^ " -> " ^ md5;
     Some (md5.[5], md5.[6]))
  | _ -> None


let rec find_digit name i =
  match try_pass name i with
  | Some (p, c) -> (p, c, i)
  | None -> find_digit name (i+1)


let rec find_digits_seq digits name i = function
  | 0 ->
    let digits = List.rev digits in
    String.init (List.length digits) (List.nth digits)
  | count ->
    let p, c, i = find_digit name i in
    find_digits_seq (p :: digits) name (i+1) (count-1)


let main_1 input =
  find_digits_seq [] input 0 8


let digits_to_str digits =
  let i_to_char i = i + Char.code '0' |> Char.chr in
  let get_dig i = try CharMap.find (i_to_char i) digits with _ -> '_' in
  String.init 8 get_dig

let set_digit digits p c =
  match p >= '0' && p <= '7' with
  | false -> (
      noise_endline @@ "  ^-- nope: " ^ (Char.escaped p);
      digits)
  | true ->
    match CharMap.mem p digits with
    | true -> (
        noise_endline @@ "  ^-- nope: " ^ (Char.escaped p);
        digits)
    | false -> (
        noise_endline @@ "  ^-- \\o/: " ^
                         (Char.escaped p) ^ " " ^ (Char.escaped c) ^
                         " " ^ (digits_to_str (CharMap.add p c digits));
        CharMap.add p c digits)

let rec find_digits_pos digits name i =
  match CharMap.cardinal digits with
  | 8 -> digits_to_str digits
  | _ ->
    let p, c, i = find_digit name i in
    let digits = set_digit digits p c in
    find_digits_pos digits name (i+1)


let main_2 input =
  find_digits_pos CharMap.empty input 0


type t = string
let parser = Parser.name
