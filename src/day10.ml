open Utils


type actor =
  | Bot of int
  | Output of int

type instruction =
  | Input of (int * actor)
  | Give of (actor * actor * actor)

let actor_to_str = function
  | Bot b -> Printf.sprintf "bot %d" b
  | Output o -> Printf.sprintf "output %d" o

let inst_to_str = function
  | Input (v, a) -> Printf.sprintf "value %d goes to %s" v (actor_to_str a)
  | Give (ag, al, ah) ->
    Printf.sprintf "%s gives low to %s and high to %s"
      (actor_to_str ag) (actor_to_str al) (actor_to_str ah)

type bot = {
  b_id : int;
  b_chips : int list;
  b_low : actor option;
  b_high : actor option;
}

type output = {
  o_id : int;
  o_chips : int list;
}

let aopt_to_str = function None -> "*" | Some a -> actor_to_str a

let bot_to_str {b_id; b_chips; b_low; b_high} =
  Printf.sprintf "Bot %d: [%s] l->%s h->%s"
    b_id (map_to_str ", " string_of_int b_chips)
    (aopt_to_str b_low) (aopt_to_str b_high)

let output_to_str {o_id; o_chips} =
  Printf.sprintf "Output %d: [%s]" o_id (map_to_str ", " string_of_int o_chips)


module Parser = struct
  open Angstrom

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let bot = string "bot " *> integer >>| fun b -> Bot b
  let output = string "output " *> integer >>| fun o -> Output o
  let actor = bot <|> output

  let input =
    lift4 (fun _ v _ a -> Input (v, a))
      (string "value ") integer (string " goes to ") actor

  let low = (string " gives low to ")
  let high = (string " and high to ")
  let give =
    lift4 (fun ag _ al _ ah -> Give (ag, al, ah))
      actor low actor high <*> actor

  let instruction = input <|> give
  let instructions = sep_by end_of_line instruction
end


let mkbot b_id = {b_id; b_chips=[]; b_low=None; b_high=None}

let add_actor (bots, outs) = function
  | Bot b -> (IntMap.add b (mkbot b) bots), outs
  | Output o -> bots, (IntMap.add o {o_id=o; o_chips=[]} outs)

let rec collect_actors actors = function
  | [] -> actors
  | Input (_, a) :: insts -> collect_actors (add_actor actors a) insts
  | Give (a1, a2, a3) :: insts ->
    collect_actors (List.fold_left add_actor actors [a1; a2; a3]) insts


let print_actors (bots, outs) =
  let botstr =
    IntMap.bindings bots |> List.map snd |> map_to_str "\n" bot_to_str
  in
  let outstr =
    IntMap.bindings outs |> List.map snd |> map_to_str "\n" output_to_str
  in
  noise_endline @@ botstr ^ "\n" ^ outstr


let set_bot_targets bots b al ah =
  let bot = IntMap.find b bots in
  let bot = { bot with b_low = Some al; b_high = Some ah } in
  IntMap.add b bot bots

let rec set_bots_targets (bots, outs) = function
  | [] -> bots, outs
  | Input _ :: insts -> set_bots_targets (bots, outs) insts
  | Give (Bot i, al, ah) :: insts ->
    let bots = set_bot_targets bots i al ah in
    set_bots_targets (bots, outs) insts
  | inst :: _ -> failwith @@ "Bad instruction: " ^ (inst_to_str inst)


let check_incomplete (bots, outs) =
  let rec check_bots count = function
    | [] -> count
    | (_, {b_chips=[]}) :: rest -> check_bots (2+count) rest
    | (_, {b_chips=[_]}) :: rest -> check_bots (1+count) rest
    | (_, {b_chips=[_; _]}) :: rest -> check_bots count rest
    | (_, bot) :: _ -> failwith @@ "Too many chips! " ^ (bot_to_str bot)
  in
  let rec check_outs count = function
    | [] -> count
    | (_, {o_chips=[]}) :: rest -> check_outs (1+count) rest
    | (_, {o_chips=[_]}) :: rest -> check_outs count rest
    | (_, out) :: _ -> failwith @@ "Too many chips! " ^ (output_to_str out)
  in
  check_bots 0 (IntMap.bindings bots) + check_outs 0 (IntMap.bindings outs)


let chip_to_actor (bots, outs) v = function
  | Bot b ->
    let bot = IntMap.find b bots in
    let b_chips = List.sort_uniq compare (v :: bot.b_chips) in
    (IntMap.add b { bot with b_chips } bots), outs
  | Output o ->
    let out = IntMap.find o outs in
    let o_chips = List.sort_uniq compare (v :: out.o_chips) in
    bots, (IntMap.add o { out with o_chips } outs)


let bot_give (bots, outs) b al ah =
  let bot = IntMap.find b bots in
  match bot.b_chips with
  | [l; h] ->
    let (bots, outs) = chip_to_actor (bots, outs) l al in
    chip_to_actor (bots, outs) h ah
  | _ -> (bots, outs)

let rec collect_chips actors = function
  | [] -> actors
  | Input (v, a) :: insts -> collect_chips (chip_to_actor actors v a) insts
  | Give (Bot b, al, ah) :: insts ->
    collect_chips (bot_give actors b al ah) insts
  | inst :: _ -> failwith @@ "Bad instruction: " ^ (inst_to_str inst)


let rec collect_all_chips actors instructions = function
  | 0 ->
    noise_endline "All chips distributed.";
    actors
  | missing ->
    noise_endline @@ "Chips still missing: " ^ (string_of_int missing);
    let actors = collect_chips actors instructions in
    let new_missing = check_incomplete actors in
    match missing - new_missing with
    | 0 -> failwith "No new chips distributed!"
    | _ -> collect_all_chips actors instructions new_missing


let print_bots_by_value (bots, outs) =
  let bots = IntMap.bindings bots |> List.map snd in
  let bcmp {b_chips=bc0} {b_chips=bc1} = compare bc0 bc1 in
  let bots = List.sort bcmp bots in
  noise_endline @@ map_to_str "\n" bot_to_str bots


let main_1 input =
  let actors = collect_actors (IntMap.empty, IntMap.empty) input in
  let actors = set_bots_targets actors input in
  let actors = collect_all_chips actors input (check_incomplete actors) in
  print_bots_by_value actors;
  let bot_17_61 =
    IntMap.bindings (fst actors) |> List.map snd |>
    List.filter (fun {b_chips=bc} -> bc = [17; 61]) |> List.hd
  in
  string_of_int bot_17_61.b_id

let main_2 input =
  let actors = collect_actors (IntMap.empty, IntMap.empty) input in
  let actors = set_bots_targets actors input in
  let actors = collect_all_chips actors input (check_incomplete actors) in
  print_actors actors;
  let get_out i = IntMap.find i (snd actors) in
  let outs_0_1_2 = [get_out 0; get_out 1; get_out 2] in
  string_of_int @@ List.fold_left (fun a {o_chips} -> List.hd o_chips * a)
    1 outs_0_1_2

type t = instruction list
let parser = Parser.instructions
