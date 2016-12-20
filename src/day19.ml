open Utils


module Parser = struct
  open Angstrom

  let number = P_misc.uinteger
end


module Q = struct
  let init_state count =
    let elves = Queue.create () in
    for i = 1 to count do Queue.add i elves done;
    elves

  let turn elves =
    let thief = Queue.take elves in
    let _victim = Queue.take elves in
    Queue.add thief elves;
    elves

  let rec run_game elves =
    match Queue.length elves with
    | 1 -> Queue.take elves
    | _ -> run_game (turn elves)
end


module Expensive = struct
  type elf = {
    num : int;
    mutable next : elf option;
  }

  type elves = {
    count : int;
    elf : elf;
  }


  let init_state count =
    let rec build elf = function
      | 0 -> elf
      | i -> build {num = i; next = Some elf} (i-1)
    in
    let last = {num = count; next = None} in
    let first = build last (count-1) in
    last.next <- Some first;
    {elf = first; count = count}

  let next elf =
    match elf.next with
    | None -> failwith "Circle broken!"
    | Some elf -> elf

  let rec nth_elf elf = function
    | 0 -> elf
    | n -> nth_elf (next elf) (n-1)

  let take_turn elf n =
    let before = nth_elf elf (n-1) in
    let after = next (next before) in
    before.next <- Some after;
    next elf

  let print_game game =
    let first = game.elf in
    let rec walk elves elf =
      match elf.num = first.num with
      | true -> elves
      | false -> walk (Printf.sprintf "%s %d" elves elf.num) (next elf)
    in
    walk (string_of_int first.num) (next first) |> noise_endline

  let rec run_game choose game =
    let n = choose game in
    ifnoise (fun () ->
        noisef "Chose %d from " (nth_elf game.elf n).num; print_game game);
    match game.count with
    | 1 -> game.elf.num
    | _ -> run_game choose {count = game.count-1; elf = take_turn game.elf n}

  let choose_next game = 1

  let choose_opposite game = game.count / 2

end


type elf = {
  num : int;
  mutable next : elf option;
}

type elves = {
  count : int;
  elf : elf;
  previctim : elf;
  vstate : int option;
}


let init_state count =
  let rec build elf = function
    | 0 -> elf
    | i -> build {num = i; next = Some elf} (i-1)
  in
  let last = {num = count; next = None} in
  let first = build last (count-1) in
  last.next <- Some first;
  {count; elf = first; previctim = first; vstate = None}

let next elf =
  match elf.next with
  | None -> failwith "Circle broken!"
  | Some elf -> elf

let rec nth_elf elf = function
  | 0 -> elf
  | n -> nth_elf (next elf) (n-1)

let print_game game =
  let first = game.elf in
  let rec walk elves elf =
    match elf.num = first.num with
    | true -> elves
    | false -> walk (Printf.sprintf "%s %d" elves elf.num) (next elf)
  in
  walk (string_of_int first.num) (next first) |> noise_endline

let take_turn {count; elf; previctim; vstate} =
  previctim.next <- Some (next (next previctim));
  {
    count = count-1;
    elf = next elf;
    previctim;
    vstate;
  }

let rec run_game choose game =
  let game = choose game in
  ifnoise (fun () ->
      noisef "Chose %d from " (next game.previctim).num; print_game game);
  match game.count with
  | 1 -> game.elf.num
  | _ -> run_game choose (take_turn game)

let choose_next game =
  match game.vstate with
  | None -> {game with vstate = Some 0}
  | Some _ -> {game with previctim = next game.previctim}

let choose_opposite game =
  let pv = game.previctim in
  match game.vstate with
  | None -> {game with
             previctim = nth_elf pv (game.count / 2 - 1);
             vstate = Some (game.count mod 2)}
  | Some 0 -> {game with vstate = Some 1}
  | Some 1 -> {game with previctim = next pv; vstate = Some 0}
  | Some _ -> failwith "Bad vstate."


let main_1 input =
  (* Q.(init_state input |> run_game) |> string_of_int *)
  (* Expensive.(init_state input |> run_game choose_next) |> string_of_int *)
  init_state input |> run_game choose_next |> string_of_int


let main_2 input =
  (* Expensive.(init_state input |> run_game choose_opposite) |> string_of_int *)
  init_state input |> run_game choose_opposite |> string_of_int


type t = int
let parser = Parser.number
