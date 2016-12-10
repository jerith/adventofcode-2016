open Utils

type netseg =
  | Super of string
  | Hyper of string

let netseg_to_str = function
  | Super seg -> seg
  | Hyper seg -> "[" ^ seg ^ "]"

let ip_to_str ip =
  map_to_str "" netseg_to_str ip


module Parser = struct
  open Angstrom

  let letters = take_while1 (function 'a' .. 'z' -> true | _ -> false)
  let super = letters >>| fun l -> Super l
  let hyper = (char '[') *> letters <* (char ']') >>| fun l -> Hyper l
  let ip = many1 (super <|> hyper)
  let ips = sep_by end_of_line ip
end


let rec find_abba s =
  match String.length s >= 4 with
  | false -> false
  | true ->
    match s.[0] != s.[1] && s.[0] == s.[3] && s.[1] == s.[2] with
    | true -> true
    | false -> find_abba (Str.string_after s 1)

let rec tls_ip super_abba = function
  | [] -> super_abba
  | Super str :: rest -> tls_ip (super_abba || find_abba str) rest
  | Hyper str :: rest ->
    match find_abba str with
    | true -> false
    | false -> tls_ip super_abba rest


let main_1 input =
  List.filter (tls_ip false) input |> List.length |> string_of_int |>
  print_endline


let rec find_abas abas babs s =
  match String.length s >= 3 with
  | false -> false, abas
  | true ->
    match s.[0] != s.[1] && s.[0] == s.[2] with
    | false -> find_abas abas babs (Str.string_after s 1)
    | true ->
      let abas = (s.[0], s.[1], s.[2]) :: abas in
      let bab = (s.[1], s.[0], s.[1]) in
      match List.mem bab babs with
      | true -> true, abas
      | false -> find_abas abas babs (Str.string_after s 1)

let rec ssl_ip abas babs = function
  | [] -> false
  | Super str :: rest ->
    (match find_abas abas babs str with
     | true, _ -> true
     | false, abas -> ssl_ip abas babs rest)
  | Hyper str :: rest ->
    (match find_abas babs abas str with
     | true, _ -> true
     | false, babs -> ssl_ip abas babs rest)


let main_2 input =
  List.filter (ssl_ip [] []) input |> List.length |> string_of_int |>
  print_endline


type t = netseg list list
let parser = Parser.ips
