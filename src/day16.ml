open Utils


module Parser = struct
  open Angstrom

  let one = char '1' *> return true
  let zero = char '0' *> return false
  let character = one <|> zero
  let seed = many1 character
end


type quarterseq = bool list * bool option list

type halfseq = int * quarterseq * quarterseq

let char2str = function true -> "1" | false -> "0"
let chars2str = map_to_str "" char2str
let extra2str = function None -> "." | Some true -> "1" | Some false -> "0"

let qs2str (cs, es) =
  Printf.sprintf "%s[%s]" (chars2str cs) (map_to_str ";" extra2str es)

let hs2str (i, ql, qr) =
  Printf.sprintf "(%d) %s" i (map_to_str "-" qs2str [ql; qr])

let rec checksum_chars csum extra = function
  | [] ->
    (* List.rev csum, (None :: extra) *)
    failwith "Can't checksum even length string"
  | [c] -> List.rev csum, (Some c :: extra)
  | c1 :: c2 :: chars ->
    checksum_chars ((c1 = c2) :: csum) extra chars

let rec checksum_qseq i (cs, es) =
  match i, (List.length cs mod 2) with
  | 0, _ | _, 0 -> i, (cs, es)
  | i, _ ->
    let cs, es = checksum_chars [] es cs in
    checksum_qseq (i-1) (cs, es)

let checksum_hseq (i, ql, qr) =
  let il, ql = checksum_qseq i ql in
  let ir, qr = checksum_qseq i qr in
  match il = ir with
  | false -> failwith "qseqs are different lengths!"
  | true -> il, ql, qr

let rec fill_extras c = function
  | [] -> c
  | None :: _ -> failwith "Can't fill with missing step."
  | Some e :: extras -> fill_extras (e = c) extras

let join_qsecs c (cl, el) (cr, er) =
  (List.concat [cl; [fill_extras c el]; cr], er)

let extend_seq (i, ql, qr) =
  let nql = join_qsecs false ql qr in
  let nqr = join_qsecs true ql qr in
  checksum_hseq (i, nql, nqr)

let len_hseq (_, (cs, _), _) = List.length cs * 2 + 1

let rec extend_to_length l (i, ql, qr) =
  noise_endline (hs2str (i, ql, qr));
  match i, len_hseq (i, ql, qr) >= l with
  | 0, true -> (i, ql, qr)
  | _ -> extend_to_length l (extend_seq (i, ql, qr))

let extract_csum l (i, ql, qr) =
  let cs, _ = join_qsecs false ql qr in
  String.sub (chars2str cs) 0 l

let first_hseq iters chars =
  let ql = chars, [] in
  let qr = List.rev_map not chars, [] in
  checksum_hseq (iters, ql, qr)

let rec sumstats iters len =
  match len mod 2 = 1 with
  | true -> iters, len
  | false -> sumstats (iters+1) (len / 2)


let main_1 input =
  let dlen = 272 in
  let iters, slen = sumstats 0 dlen in
  noisef "%d -> %d, %d\n" dlen iters slen;
  let hseq = first_hseq iters input |> extend_to_length slen in
  extract_csum slen hseq

let main_2 input =
  let dlen = 35651584 in
  let iters, slen = sumstats 0 dlen in
  noisef "%d -> %d, %d\n" dlen iters slen;
  let hseq = first_hseq iters input |> extend_to_length slen in
  extract_csum slen hseq


type t = bool list
let parser = Parser.seed
