open Semantic_check
open Gen_python

let _ =
  (* let lexbuf = Lexing.from_channel stdin in *)
    let lexbuf = Lexing.from_string 
"int DFA main(){
        int counter = 0; 
        stack<int> countDown;
        start{
            print(\"Hello World!\");

            state3<- (counter > 10);
            state2<-*;}
        state2{
            print(\"state2\\n\");
            int blah = 0;
            counter=(counter + 1);
            start<-*;}
        state3{
            print(\"state3\\n\");
            //print(red());
            state4<- (counter < 1);
            counter = counter - 1;
            countDown.push(1);
            state3 <-*;
            }
        state4{
            state5<-(countDown.peek == EOS);
            print(\"popping like cray\");
            countDown.pop;
            state4<-*;
            }
        state5{
            print(\"Donezoes\");
            return 0;
            }
}
/*string DFA red() {
    string fire = \"Fire\";
    start{
        print(\"Called red()\");
        return fire;
        }
}*/
" in
  let ast = Parser.program Scanner.token lexbuf in
  let sast = Semantic_check.check_program ast in
  let code = gen_program sast in
  let output = open_out "output.py" in
  output_string output code
