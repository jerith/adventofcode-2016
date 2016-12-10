let parse_input_file a_parser filename =
  let module AB = Angstrom.Buffered in
  let pstate = AB.parse a_parser in
  let inch = open_in_bin filename in
  let buf = Bytes.create 4096 in
  let rec read pst =
    match input inch buf 0 4096 with
    | 0 -> (match AB.state_to_result (AB.feed pst `Eof) with
        | Result.Ok v -> v
        | Result.Error msg -> failwith msg)
    | n -> read @@ AB.feed pst @@ `String (Bytes.sub_string buf 0 n)
  in read pstate