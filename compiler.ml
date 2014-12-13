open Semantic_check
open Gen_python

let _ =
  (* let lexbuf = Lexing.from_channel stdin in *)
  let lexbuf = Lexing.from_string "int DFA main(){start{print(\"Hello World!\");
  return 3; start <- *;}}" in
  let ast = Parser.program Scanner.token lexbuf in
  let sast = Semantic_check.check_program ast in
  let code = gen_program sast in
  let output = open_out "output.py" in
  output_string output code
