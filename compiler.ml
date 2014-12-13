open Semantic_check
open Gen_python

let _ =
  (* let lexbuf = Lexing.from_channel stdin in *)
    let lexbuf = Lexing.from_string 
    "int DFA main(){
        int counter = 0; 
        start{
            print(\"Hello World!\");
            state3<- (counter > 10);
            state2<-*;}
        state2{
            print(\"state2\n\");
            counter=(counter + 1);
            start<-*;}
        state3{
            print(\"state3\n\");
            return 10;}
}" in
  let ast = Parser.program Scanner.token lexbuf in
  let sast = Semantic_check.check_program ast in
  let code = gen_program sast in
  let output = open_out "output.py" in
  output_string output code
