
let parse_file filename =
  let input_file = (open_in filename) in
  let lexbuf = Lexing.from_channel input_file in
  let input_program = Parser.start Lexer.read lexbuf in
  input_program
