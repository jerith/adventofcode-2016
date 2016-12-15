let char_map = [

  'A', [" ##  ";
        "#  # ";
        "#  # ";
        "#### ";
        "#  # ";
        "#  # "];

  'B', ["###  ";
        "#  # ";
        "###  ";
        "#  # ";
        "#  # ";
        "###  "];

  'C', [" ##  ";
        "#  # ";
        "#    ";
        "#    ";
        "#  # ";
        " ##  "];

  'D', ["###  ";
        "#  # ";
        "#  # ";
        "#  # ";
        "#  # ";
        "###  "];

  'E', ["#### ";
        "#    ";
        "###  ";
        "#    ";
        "#    ";
        "#### "];

  'F', ["#### ";
        "#    ";
        "###  ";
        "#    ";
        "#    ";
        "#    "];

  'G', [" ##  ";
        "#  # ";
        "#    ";
        "# ## ";
        "#  # ";
        " ### "];

  'H', ["#  # ";
        "#  # ";
        "#### ";
        "#  # ";
        "#  # ";
        "#  # "];

  'I', [" ### ";
        "  #  ";
        "  #  ";
        "  #  ";
        "  #  ";
        " ### "];

  'J', ["  ## ";
        "   # ";
        "   # ";
        "   # ";
        "#  # ";
        " ##  "];

  'K', ["#  # ";
        "# #  ";
        "##   ";
        "# #  ";
        "# #  ";
        "#  # "];

  'L', ["#    ";
        "#    ";
        "#    ";
        "#    ";
        "#    ";
        "#### "];

  'M', ["-----";
        "-----";
        "-----";
        "-----";
        "-----";
        "-----"];

  'N', ["-----";
        "-----";
        "-----";
        "-----";
        "-----";
        "-----"];

  'O', [" ##  ";
        "#  # ";
        "#  # ";
        "#  # ";
        "#  # ";
        " ##  "];

  'P', ["###  ";
        "#  # ";
        "#  # ";
        "###  ";
        "#    ";
        "#    "];

  'Q', ["-----";
        "-----";
        "-----";
        "-----";
        "-----";
        "-----"];

  'R', ["###  ";
        "#  # ";
        "#  # ";
        "###  ";
        "# #  ";
        "#  # "];

  'S', [" ### ";
        "#    ";
        "#    ";
        " ##  ";
        "   # ";
        "###  "];

  'T', ["#####";
        "  #  ";
        "  #  ";
        "  #  ";
        "  #  ";
        "  #  "];

  'U', ["#  # ";
        "#  # ";
        "#  # ";
        "#  # ";
        "#  # ";
        " ##  "];

  'V', ["#   #";
        "#   #";
        "#   #";
        " # # ";
        " # # ";
        "  #  "];

  'W', ["-----";
        "-----";
        "-----";
        "-----";
        "-----";
        "-----"];

  'X', ["-----";
        "-----";
        "-----";
        "-----";
        "-----";
        "-----"];

  'Y', ["#   #";
        "#   #";
        " # # ";
        "  #  ";
        "  #  ";
        "  #  "];

  'Z', ["#### ";
        "   # ";
        "  #  ";
        " #   ";
        "#    ";
        "#### "];

  (* Technically not part of the spec, but useful. *)
  ' ', ["     ";
        "     ";
        "     ";
        "     ";
        "     ";
        "     "];
]

let map_char = List.map (fun (c, l) -> (l, c)) char_map



let empty_output = [""; ""; ""; ""; ""; ""]


let append_char output char =
  let font_char = List.assoc char char_map in
  List.map2 (^) output font_char

let fontify str =
  List.fold_left append_char empty_output (Utils.list_of_chars str) |>
  String.concat "\n"


let decode_char font_char =
  try List.assoc font_char map_char
  with Not_found ->
    print_endline @@ String.concat "\n" ("Can't decode:" :: font_char);
    failwith "Can't decode."

let read_char input =
  let split_row row = Str.string_before row 5, Str.string_after row 5 in
  let font_char, input = List.map split_row input |> List.split in
  decode_char font_char, input


let parse_chars input =
  let rec parse chars = function
    | [""; ""; ""; ""; ""; ""] -> chars
    | input ->
      let char, input = read_char input in
      parse (char :: chars) input
  in
  let rows = Str.split (Str.regexp_string "\n") input in
  parse [] rows |> List.rev |> Utils.map_to_str "" Char.escaped
