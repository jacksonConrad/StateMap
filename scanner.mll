{ open Parser }

rule token = 
	parse [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Multi-line comment *)
| "//"     { singleComment lexbuf }     (* Single-line comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ':'      { COLON }
| ','      { COMMA }
| '.'      { DOT }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { STAR }
| '/'      { DIVIDE }
| '%'      { MOD }
| '='      { ASSIGN }
| "=="     { EQ }
| "++"	   { INC }
| "--"	   { DEC }
| '!'      { NOT }
| "!="     { NEQ }
| "&&"     { AND }
| "||"     { OR }
| '<'      { LT }
| "<-"     { TRANS }
| "<="     { LEQ }
| '>'      { GT }
| ">="     { GEQ }
| "return" { RETURN }
| "int"    { INT }
| "double" { DOUBLE }
| "string" { STRING }
| "void"   { VOID }
| "DFA"    { DFA }
| "main"   { MAIN }
| "stack"  { STACK }
| ['0'-'9']+ as lxm { INT_LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| '"' (('\\' _  | [^'"'])* as lxm) '"'{ STRING_LITERAL(lxm) }
| eof      { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and singleComment = parse
  '\n' { token lexbuf }
| eof  { EOF }
| _    { singleComment lexbuf }
