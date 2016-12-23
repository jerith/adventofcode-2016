open Utils


type register = char

type operand =
  | Int of int
  | Reg of register

type instruction =
  | CPY of (operand * register)
  | INC of register
  | DEC of register
  | JNZ of (operand * operand)
  | TGL of operand
  | InvalidToggled of instruction

type optop =
  | Add of (register * register)
  | Mul of (register * register * register * operand)


let reg_to_str = Char.escaped

let operand_to_str = function
  | Int i -> string_of_int i
  | Reg r -> reg_to_str r

let spjoin = String.concat " "

let rec instr_to_str = function
  | CPY (o, r) -> spjoin ["cpy"; operand_to_str o; reg_to_str r]
  | INC r -> spjoin ["inc"; reg_to_str r]
  | DEC r -> spjoin ["dec"; reg_to_str r]
  | JNZ (o0, o1) -> spjoin ["jnz"; operand_to_str o0; operand_to_str o1]
  | TGL o -> spjoin ["tgl"; operand_to_str o]
  | InvalidToggled op -> spjoin ["INVALID TOGGLE OF"; instr_to_str op]


module Parser = struct
  open Angstrom

  let spaces = many1 (char ' ')

  let uinteger =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
  let sign = (char '-' *> return (-1)) <|> (return 1)
  let integer = lift2 (fun s i -> s * i) sign uinteger

  let register = choice @@ List.map char ['a'; 'b'; 'c'; 'd']

  let intop = integer >>| fun i -> Int i
  let regop = register >>| fun r -> Reg r

  let operand = intop <|> regop

  let op2 name o f = string name *> spaces *> o >>| f
  let op3 name o1 o2 f = lift2 f (string name *> spaces *> o1) (spaces *> o2)

  let instruction = choice [
      op3 "cpy" operand register (fun o r -> CPY (o, r));
      op2 "inc" register (fun r -> INC r);
      op2 "dec" register (fun r -> DEC r);
      op3 "jnz" operand operand (fun o0 o1 -> JNZ (o0, o1));
      op2 "tgl" operand (fun o -> TGL o)]
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
  noise_endline @@ String.concat "\n" [
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

let get_value rs = function
  | Reg r -> CharMap.find r rs
  | Int i -> i

let get_op computer offset = computer.instructions.(computer.ip + offset)

let update_reg rs f r =
  CharMap.add r (f @@ CharMap.find r rs) rs

let toggle computer offset =
  let ti = computer.ip + offset in
  match ti >= 0 && ti < Array.length computer.instructions with
  | false -> computer.instructions
  | true ->
    let newop =
      match computer.instructions.(ti) with
      | CPY (o, r) -> JNZ (o, Reg r)
      | INC r -> DEC r
      | DEC r -> INC r
      | JNZ (o0, o1) ->
        (match o1 with
         | Int _ -> InvalidToggled (JNZ (o0, o1))
         | Reg r -> CPY (o0, r))
      | TGL o ->
        (match o with
         | Int _ -> InvalidToggled (TGL o)
         | Reg r -> INC r)
      | InvalidToggled op ->
        (match op with
         | JNZ _ -> op
         | TGL _ -> InvalidToggled op
         | _ -> failwith "ONLY jnz AND tgl CAN TOGGLE TO INVALID VALUES")
    in
    Array.mapi (fun i op -> if i = ti then newop else op) computer.instructions

let exec computer =
  let rs = computer.registers in
  match get_op computer 0 with
  | CPY (o, r) -> {
      computer with
      registers = update_reg rs (fun _ -> get_value rs o) r;
      ip = computer.ip + 1}
  | INC r -> {
      computer with
      registers = update_reg rs (fun v -> v+1) r;
      ip = computer.ip + 1}
  | DEC r -> {
      computer with
      registers = update_reg rs (fun v -> v-1) r;
      ip = computer.ip + 1}
  | JNZ (o0, o1) -> {
      computer with
      ip = computer.ip + (if get_value rs o0 = 0 then 1 else get_value rs o1)}
  | TGL o -> {
      computer with
      instructions = toggle computer (get_value rs o);
      ip = computer.ip + 1}
  | InvalidToggled _ -> {
      computer with
      ip = computer.ip + 1}

let check_add = function
  | INC ri, DEC rd, JNZ (Reg rj, Int -2) when rd = rj -> Some (ri, rd)
  | DEC rd, INC ri, JNZ (Reg rj, Int -2) when rd = rj -> Some (ri, rd)
  | _ -> None

let find_add computer =
  let get_op = get_op computer in
  match computer.ip < Array.length computer.instructions - 2 with
  | false -> None
  | true -> match check_add (get_op 0, get_op 1, get_op 2) with
    | None -> None
    | Some (ri, rd) -> Some (Add (ri, rd))

let check_mul (op0, op1, op2, op3, op4, op5) =
  match check_add (op1, op2, op3) with
  | None -> None
  | Some (ri, rd) ->
    match op0, op4, op5 with
    | CPY (o, rc), DEC rm, JNZ (Reg rj, Int -5) when rc = rd && rm = rj ->
      Some (Mul (ri, rd, rm, o))
    | _ -> None

let find_mul computer =
  let get_op = get_op computer in
  match computer.ip < Array.length computer.instructions - 5 with
  | false -> None
  | true ->
    check_mul (get_op 0, get_op 1, get_op 2, get_op 3, get_op 4, get_op 5)

let rec find_opt computer = function
  | [] -> None
  | f :: opts -> match f computer with
    | Some opt -> Some opt
    | None -> find_opt computer opts

let add_reg computer ri rd =
  let rs = computer.registers in
  let rs = setreg (get_value rs (Reg ri) + get_value rs (Reg rd)) rs ri in
  setreg 0 rs rd

let mul_reg computer ri rd rm o =
  let rs = computer.registers in
  let prod = get_value rs o * get_value rs (Reg rm) in
  let rs = setreg (get_value rs (Reg ri) + prod) rs ri in
  let rs = setreg 0 rs rd in
  setreg 0 rs rm

let exec_opt computer =
  match find_opt computer [find_add; find_mul] with
  | Some (Add (ri, rd)) -> {
      computer with
      registers = add_reg computer ri rd;
      ip = computer.ip + 3}
  | Some (Mul (ri, rd, rm, o)) -> {
      computer with
      registers = mul_reg computer ri rd rm o;
      ip = computer.ip + 6}
  | None -> exec computer

let rec run computer =
  ifnoise (fun () -> print_computer computer);
  match computer.ip >= Array.length computer.instructions with
  | true -> computer
  | false -> exec_opt computer |> run


let main_1 input =
  let computer = boot input in
  let computer = {computer with registers = setreg 7 computer.registers 'a'} in
  let computer = run computer in
  CharMap.find 'a' computer.registers |> string_of_int


let main_2 input =
  let computer = boot input in
  let computer = {computer with registers = setreg 12 computer.registers 'a'} in
  let computer = run computer in
  CharMap.find 'a' computer.registers |> string_of_int


type t = instruction list
let parser = Parser.instructions
