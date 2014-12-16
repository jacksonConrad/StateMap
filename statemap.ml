type action = Ast 

let _ =
  let action = Ast in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
   Ast -> let listing = Ast.string_of_program program
           in print_string listing
