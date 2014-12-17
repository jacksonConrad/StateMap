open Semantic_check
open Gen_python
open Sys

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  let sast = Semantic_check.check_program ast in
  let code = gen_program sast in
  let output = open_out (Sys.argv.(1) ^ ".py") in
  output_string output code
