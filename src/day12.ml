open Utils


type register = char

type operand =
  | Integer of int
  | Register of register


type instruction =
  | Copy of (operand * register)
  | Increment of register
  | Decrement of register
  | JumpNZ of (operand * int)


let reg_to_str = Char.escaped

let operand_to_str = function
  | Integer i -> string_of_int i
  | Register r -> reg_to_str r

let instr_to_str = function
  | Copy (o, r) -> String.concat " " ["cpy"; operand_to_str o; reg_to_str r]
  | Increment r -> "inc " ^ (reg_to_str r)
  | Decrement r -> "dec " ^ (reg_to_str r)
  | JumpNZ (o, i) ->
    String.concat " " ["jnz"; operand_to_str o; string_of_int i]

module Parser = struct
  open Angstrom

  let spaces = many1 (char ' ')

  let uinteger =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
  let sign = (char '-' *> return (-1)) <|> (return 1)
  let integer = lift2 (fun s i -> s * i) sign uinteger

  let register = choice @@ List.map char ['a'; 'b'; 'c'; 'd']

  let intop = integer >>| fun i -> Integer i
  let regop = register >>| fun r -> Register r

  let operand = intop <|> regop

  let op2 name o f = string name *> spaces *> o >>| f
  let op3 name o1 o2 f = lift2 f (string name *> spaces *> o1) (spaces *> o2)

  let cpy = op3 "cpy" operand register (fun o r -> Copy (o, r))
  let inc = op2 "inc" register (fun r -> Increment r)
  let dec = op2 "dec" register (fun r -> Decrement r)
  let jnz = op3 "jnz" operand integer (fun o i -> JumpNZ (o, i))

  let instruction = choice [cpy; inc; dec; jnz]
  let instructions = sep_by end_of_line instruction
end


type computer = {
  registers : int CharMap.t;
  instructions : instruction array;
  ip : int;
}


let print_computer {registers; instructions; ip} =
  let reg_to_str (r, v) = Printf.sprintf "  %c = %d" r v in
  let reg_strs = List.map reg_to_str (CharMap.bindings registers) in
  let inst_to_str i inst =
    Printf.sprintf " %c %s" (if i = ip then '*' else ' ') (instr_to_str inst)
  in
  let inst_strs = List.mapi inst_to_str (Array.to_list instructions) in
  print_endline @@ String.concat "\n" [
    "-------------------";
    "Registers:";
    String.concat "\n" reg_strs;
    "Instructions:";
    String.concat "\n" inst_strs;
  ]


let setreg v rs r = CharMap.add r v rs

let boot input = {
  registers = List.fold_left (setreg 0) CharMap.empty ['a'; 'b'; 'c'; 'd'];
  instructions = Array.of_list input;
  ip = 0;
}


let operand_value rs = function
  | Register r -> CharMap.find r rs
  | Integer i -> i

let update_reg rs f r =
  CharMap.add r (f @@ CharMap.find r rs) rs

let exec computer =
  let rs = computer.registers in
  match (computer.instructions.(computer.ip)) with
  | Copy (o, r) -> {
      computer with
      registers = update_reg rs (fun _ -> operand_value rs o) r;
      ip = computer.ip + 1}
  | Increment r -> {
      computer with
      registers = update_reg rs (fun v -> v+1) r;
      ip = computer.ip + 1}
  | Decrement r -> {
      computer with
      registers = update_reg rs (fun v -> v-1) r;
      ip = computer.ip + 1}
  | JumpNZ (o, i) -> {
      computer with
      ip = computer.ip + (if operand_value rs o = 0 then 1 else i)}


let rec run computer =
  (* print_computer computer; *)
  match computer.ip >= Array.length computer.instructions with
  | true -> computer
  | false -> exec computer |> run


let main_1 input =
  let computer = boot input in
  let computer = run computer in
  Printf.printf "\na = %d\n" (CharMap.find 'a' computer.registers)


let main_2 input =
  let computer = boot input in
  let computer = {computer with registers = setreg 1 computer.registers 'c'} in
  let computer = run computer in
  Printf.printf "\na = %d\n" (CharMap.find 'a' computer.registers)

type t = instruction list
let parser = Parser.instructions
