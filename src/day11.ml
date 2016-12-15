open Utils


type device =
  | Microchip of string
  | Generator of string

type floor =
  | Floor of (string * device list)

let device_to_str = function
  | Microchip kind -> Printf.sprintf "%s-compatible microchip" kind
  | Generator kind -> Printf.sprintf "%s generator" kind

let floor_to_str = function
  | Floor (name, devices) ->
    Printf.sprintf "The %s floor: [%s]"
      name (map_to_str ", " device_to_str devices)

let cmp_devices d0 d1 =
  let astuple = function Generator k -> (k, 0) | Microchip k -> (k, 1) in
  compare (astuple d0) (astuple d1)

let sort_devs devices = List.sort cmp_devices devices


module Parser = struct
  open Angstrom

  let letters =
    take_while1 (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)

  let floor_name = string "The " *> letters <* string " floor contains "

  let microchip =
    string "a " *> letters <* string "-compatible microchip" >>|
    fun kind -> Microchip kind

  let generator =
    string "a " *> letters <* string " generator" >>|
    fun kind -> Generator kind

  let device = microchip <|> generator

  let devsep = choice @@ List.map string [" and "; ", and "; ", "]
  let devlist = sep_by1 devsep device
  let floor_content = devlist <|> (string "nothing relevant" >>| fun _ -> [])

  let floor =
    lift3 (fun name content _ -> Floor (name, content))
      floor_name floor_content (char '.')

  let floors = sep_by end_of_line floor
end


type rtfloor = {
  fnum : int;
  fname : string;
  devices : device list;
}

type rtf = {
  floors : rtfloor list;
  kinds : string list;
  elevator : int;
}


let dev_to_blockstr = function
  | Generator kind -> String.sub kind 0 2 ^ "G"
  | Microchip kind -> String.sub kind 0 2 ^ "M"

let remove_kind kind = List.filter (fun k -> k <> kind)

let rec check_floor_safe gens mics = function
  | [] -> (List.length gens = 0) || (List.length mics = 0)
  | Generator kind :: devices ->
    (match List.mem kind mics with
     | true -> check_floor_safe gens (remove_kind kind mics) devices
     | false -> check_floor_safe (kind :: gens) mics devices)
  | Microchip kind :: devices ->
    (match List.mem kind gens with
     | true -> check_floor_safe (remove_kind kind gens) mics devices
     | false -> check_floor_safe gens (kind :: mics) devices)


let print_rtf_floor rtf floor =
  let fnum = "F" ^ (string_of_int floor.fnum) in
  let safe = if check_floor_safe [] [] floor.devices then " " else "!" in
  let elev = if floor.fnum == rtf.elevator then "E" else "." in
  let all_devices =
    List.map (fun k -> [Generator k; Microchip k]) rtf.kinds |> List.concat
  in
  let dev_block device =
    match List.mem device floor.devices with
    | true -> dev_to_blockstr device
    | false -> " . "
  in
  let device_blocks = List.map dev_block all_devices in
  noise_endline @@ String.concat " " (fnum :: safe :: elev :: device_blocks)

let print_rtf rtf =
  List.iter (fun f -> print_rtf_floor rtf f) rtf.floors

let device_kind = function
  | Generator kind -> kind
  | Microchip kind -> kind

let rec build_rtf rtf fnum = function
  | [] -> rtf
  | Floor (fname, devices) :: rest ->
    let kinds = List.map device_kind devices @ rtf.kinds in
    let devices = sort_devs devices in
    build_rtf {rtf with
               floors = {fnum; fname; devices} :: rtf.floors;
               kinds = List.sort_uniq compare kinds;
              } (fnum+1) rest

let get_floor rtf fnum = List.nth rtf.floors (List.length rtf.floors - fnum)

let check_safe rtf =
  let check floor = check_floor_safe [] [] floor.devices in
  List.for_all check rtf.floors

let update_floor rtf floor =
  let update f = if f.fnum = floor.fnum then floor else f in
  {rtf with floors = List.map update rtf.floors}

let remove_device rtf fnum device =
  let floor = get_floor rtf fnum in
  match List.mem device floor.devices with
  | false -> failwith "Can't remove device from where it isn't!"
  | true ->
    let devices = List.filter (fun d -> d <> device) floor.devices in
    update_floor rtf {floor with devices = sort_devs devices}

let add_device rtf fnum device =
  let floor = get_floor rtf fnum in
  match List.mem device floor.devices with
  | true -> failwith "Can't add a device to where it already is!"
  | false ->
    let devices = device :: floor.devices in
    update_floor rtf {floor with devices = sort_devs devices}

let move_device src dst rtf device =
  match src = rtf.elevator, abs (src - dst) with
  | true, 1 ->
    let rtf = remove_device rtf src device in
    add_device rtf dst device
  | false, _ -> failwith "Elevator not on this floor!"
  | _, _ -> failwith "Destination too far away!"

exception Unsafe_move

let move_devices rtf src dst devices =
  noisef "%d -> %d [%s]\n"
    src dst (map_to_str ", " dev_to_blockstr devices);
  match devices with
  | [] -> failwith "Elevator won't operate when empty."
  | [_] | [_; _] ->
    let rtf = List.fold_left (move_device src dst) rtf devices in
    let rtf = {rtf with elevator=dst} in
    (match check_safe rtf with
     | true -> rtf
     | false -> print_rtf rtf; raise Unsafe_move)
  | _ -> failwith "Elevator over capacity."


let devices_below rtf =
  let check_floor f = f.fnum < rtf.elevator && List.length f.devices > 0 in
  List.fold_left (fun found f -> found || check_floor f) false rtf.floors


let try_move rtf src dst devs =
  try true, move_devices rtf src dst devs with Unsafe_move -> (false, rtf)

let rec auto_move n rtf =
  print_rtf rtf;
  noisef "Move %d: " (n+1);
  (if devices_below rtf then auto_move_below else auto_move_nobelow) n rtf

and auto_move_below n rtf =
  let e, floor = rtf.elevator, get_floor rtf rtf.elevator in
  let rec amb = function
    | [] -> false, n
    | device :: devices ->
      match try_move rtf e (e-1) [device] with
      | true, rtf -> auto_move (n+1) rtf
      | false, _ ->
        noise_endline "backtrack!";
        amb devices
  in
  amb floor.devices

and auto_move_nobelow n rtf =
  let rec all_pairs pairs = function
    | [] -> pairs
    | hd :: tl ->
      let pairs = List.fold_left (fun xs x -> [hd; x] :: xs) pairs tl in
      all_pairs pairs tl
  in
  let e, floor = rtf.elevator, get_floor rtf rtf.elevator in
  let rec amn = function
    | [] -> false, n
    | pair :: pairs ->
      match try_move rtf e (e+1) pair with
      | true, rtf -> auto_move (n+1) rtf
      | false, _ ->
        noise_endline "backtrack!";
        amn pairs
  in
  match e with
  | 4 -> noise_endline "Done, no need to move."; true, n
  | _ ->
    match floor.devices with
    | [] -> failwith "Nothing on this floor"
    | [device] ->
      auto_move (n+1) @@ move_devices rtf e (e+1) [device]
    | devices -> amn (all_pairs [] devices)



let main_1 input =
  map_to_str "\n" floor_to_str input |> noise_endline;
  let rtf = {floors=[]; kinds=[]; elevator=1} in
  let rtf = build_rtf rtf 1 input in
  match auto_move 0 rtf with
  | true, n -> string_of_int n
  | false, _ -> failwith "no solution found"


let add_to_floor floor rtf device =
  let kinds = List.sort_uniq compare (device_kind device :: rtf.kinds) in
  let rtf = {rtf with kinds} in
  add_device rtf floor device


let main_2 input =
  map_to_str "\n" floor_to_str input |> noise_endline;
  let rtf = {floors=[]; kinds=[]; elevator=1} in
  let rtf = build_rtf rtf 1 input in
  let rtf =
    List.fold_left (add_to_floor 1) rtf [
      Generator "elerium";
      Microchip "elerium";
      Generator "dilithium";
      Microchip "dilithium";
    ]
  in
  match auto_move 0 rtf with
  | true, n -> string_of_int n
  | false, _ -> failwith "no solution found"

type t = floor list
let parser = Parser.floors
