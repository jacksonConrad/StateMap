open Semantic_check

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  let sast = Semantic_check.check_program ast in
  let code = gen_program sast in
  let output = open_out "output.py" in
  output_string output code