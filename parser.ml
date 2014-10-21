type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | RBRAC
  | LBRAC
  | COLON
  | DOT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | STAR
  | NOT
  | INC
  | DEC
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | OR
  | AND
  | MOD
  | RETURN
  | TRANS
  | KEYS
  | DFA
  | STACK
  | MAP
  | INT_LITERAL of (int)
  | STRING_LITERAL of (string)
  | TYPE of (string)
  | ID of (string)
  | EOF
  | MAIN
  | STRING
  | INT
  | VOID

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 2 "parser.mly"
 open Type 
# 53 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* COMMA *);
  263 (* RBRAC *);
  264 (* LBRAC *);
  265 (* COLON *);
  266 (* DOT *);
  267 (* PLUS *);
  268 (* MINUS *);
  269 (* TIMES *);
  270 (* DIVIDE *);
  271 (* ASSIGN *);
  272 (* STAR *);
  273 (* NOT *);
  274 (* INC *);
  275 (* DEC *);
  276 (* EQ *);
  277 (* NEQ *);
  278 (* LT *);
  279 (* LEQ *);
  280 (* GT *);
  281 (* GEQ *);
  282 (* OR *);
  283 (* AND *);
  284 (* MOD *);
  285 (* RETURN *);
  286 (* TRANS *);
  287 (* KEYS *);
  288 (* DFA *);
  289 (* STACK *);
  290 (* MAP *);
    0 (* EOF *);
  295 (* MAIN *);
  296 (* STRING *);
  297 (* INT *);
  298 (* VOID *);
    0|]

let yytransl_block = [|
  291 (* INT_LITERAL *);
  292 (* STRING_LITERAL *);
  293 (* TYPE *);
  294 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\007\000\007\000\007\000\007\000\008\000\
\008\000\003\000\005\000\005\000\010\000\010\000\010\000\010\000\
\011\000\011\000\006\000\006\000\013\000\014\000\009\000\009\000\
\015\000\015\000\004\000\016\000\016\000\016\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\000\000"

let yylen = "\002\000\
\001\000\002\000\009\000\001\000\001\000\001\000\001\000\001\000\
\001\000\010\000\000\000\003\000\002\000\004\000\004\000\006\000\
\001\000\003\000\000\000\005\000\001\000\003\000\000\000\001\000\
\001\000\003\000\002\000\000\000\003\000\001\000\001\000\001\000\
\004\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\002\000\002\000\002\000\004\000\003\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\006\000\007\000\005\000\004\000\009\000\
\055\000\001\000\000\000\008\000\000\000\000\000\002\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\027\000\025\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\000\000\000\000\000\000\000\000\000\000\000\000\000\
\003\000\000\000\000\000\012\000\000\000\000\000\000\000\021\000\
\000\000\000\000\000\000\000\000\000\000\031\000\032\000\000\000\
\000\000\000\000\010\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\048\000\049\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\022\000\020\000\018\000\000\000\
\053\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\054\000\052\000\033\000\029\000"

let yydgoto = "\002\000\
\009\000\010\000\011\000\019\000\030\000\036\000\057\000\013\000\
\025\000\032\000\050\000\090\000\047\000\048\000\026\000\091\000"

let yysindex = "\001\000\
\089\255\000\000\241\254\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\089\255\000\000\017\255\050\255\000\000\015\255\
\116\255\052\255\056\255\031\255\116\255\051\255\000\000\000\000\
\071\255\072\255\116\255\075\255\116\255\043\255\044\255\083\255\
\116\255\000\000\086\255\082\255\254\254\116\255\043\255\064\255\
\000\000\057\255\030\255\000\000\095\255\030\255\096\255\000\000\
\109\255\087\255\030\255\030\255\030\255\000\000\000\000\010\255\
\115\255\252\000\000\000\243\255\043\255\057\255\030\255\106\000\
\244\254\244\254\030\255\030\255\030\255\030\255\030\255\030\255\
\030\255\000\000\000\000\030\255\030\255\030\255\030\255\030\255\
\030\255\030\255\030\255\030\255\000\000\000\000\000\000\252\000\
\000\000\212\000\125\255\234\000\132\000\032\255\032\255\244\254\
\244\254\014\001\014\001\032\001\032\001\032\001\032\001\003\255\
\003\255\244\254\030\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\129\255\000\000\000\000\000\000\
\000\000\140\255\006\255\000\000\000\000\146\255\000\000\000\000\
\006\255\000\000\000\000\000\000\117\255\006\255\146\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\004\255\144\255\000\000\000\000\000\000\000\000\000\000\085\255\
\000\000\145\255\000\000\000\000\146\255\000\000\000\000\000\000\
\113\255\141\255\071\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\157\255\
\000\000\156\255\000\000\000\000\000\000\015\000\042\000\169\255\
\197\255\055\255\199\000\090\000\160\000\167\000\192\000\054\000\
\081\000\225\255\071\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\149\000\000\000\000\000\051\000\010\000\218\255\056\000\000\000\
\000\000\000\000\109\000\213\255\000\000\000\000\000\000\066\000"

let yytablesize = 572
let yytable = "\058\000\
\045\000\001\000\060\000\042\000\017\000\074\000\075\000\064\000\
\065\000\066\000\011\000\067\000\043\000\070\000\071\000\072\000\
\073\000\068\000\017\000\088\000\074\000\075\000\086\000\014\000\
\092\000\093\000\094\000\095\000\096\000\097\000\084\000\051\000\
\098\000\099\000\100\000\101\000\102\000\103\000\104\000\105\000\
\106\000\052\000\039\000\011\000\072\000\073\000\053\000\044\000\
\016\000\074\000\075\000\017\000\018\000\021\000\027\000\039\000\
\012\000\039\000\022\000\084\000\039\000\039\000\004\000\005\000\
\054\000\055\000\012\000\056\000\023\000\006\000\007\000\024\000\
\020\000\028\000\039\000\039\000\020\000\029\000\033\000\034\000\
\035\000\037\000\031\000\038\000\020\000\034\000\041\000\034\000\
\031\000\040\000\034\000\034\000\046\000\031\000\049\000\034\000\
\034\000\034\000\034\000\059\000\061\000\063\000\034\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\050\000\062\000\050\000\069\000\013\000\050\000\050\000\
\003\000\004\000\005\000\050\000\050\000\050\000\050\000\108\000\
\006\000\007\000\008\000\023\000\050\000\050\000\050\000\050\000\
\050\000\050\000\050\000\050\000\050\000\051\000\024\000\051\000\
\014\000\015\000\051\000\051\000\004\000\005\000\019\000\051\000\
\051\000\051\000\051\000\006\000\007\000\016\000\030\000\015\000\
\051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
\051\000\037\000\087\000\037\000\111\000\000\000\037\000\037\000\
\000\000\000\000\000\000\037\000\037\000\037\000\037\000\000\000\
\000\000\000\000\000\000\000\000\037\000\037\000\037\000\037\000\
\037\000\037\000\037\000\037\000\037\000\038\000\000\000\038\000\
\000\000\000\000\038\000\038\000\000\000\000\000\000\000\038\000\
\038\000\038\000\038\000\000\000\000\000\000\000\000\000\000\000\
\038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
\038\000\045\000\000\000\045\000\000\000\000\000\045\000\045\000\
\000\000\000\000\000\000\045\000\045\000\045\000\045\000\000\000\
\000\000\000\000\000\000\085\000\045\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\045\000\070\000\071\000\072\000\
\073\000\000\000\000\000\000\000\074\000\075\000\076\000\077\000\
\078\000\079\000\080\000\081\000\082\000\083\000\084\000\035\000\
\000\000\035\000\000\000\000\000\035\000\035\000\000\000\000\000\
\000\000\035\000\035\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\035\000\035\000\035\000\035\000\035\000\035\000\
\035\000\035\000\036\000\000\000\036\000\000\000\000\000\036\000\
\036\000\000\000\000\000\000\000\036\000\036\000\047\000\000\000\
\047\000\000\000\000\000\047\000\047\000\036\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\000\000\000\000\000\000\
\000\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
\047\000\046\000\000\000\046\000\000\000\000\000\046\000\046\000\
\000\000\000\000\041\000\000\000\041\000\000\000\000\000\041\000\
\041\000\000\000\000\000\000\000\046\000\046\000\046\000\046\000\
\046\000\046\000\046\000\046\000\089\000\041\000\041\000\041\000\
\041\000\041\000\041\000\000\000\070\000\071\000\072\000\073\000\
\000\000\000\000\000\000\074\000\075\000\076\000\077\000\078\000\
\079\000\080\000\081\000\082\000\083\000\084\000\110\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\070\000\071\000\
\072\000\073\000\000\000\000\000\000\000\074\000\075\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\083\000\084\000\
\042\000\000\000\042\000\000\000\000\000\042\000\042\000\043\000\
\000\000\043\000\000\000\000\000\043\000\043\000\000\000\000\000\
\000\000\000\000\000\000\042\000\042\000\042\000\042\000\042\000\
\042\000\000\000\043\000\043\000\043\000\043\000\043\000\043\000\
\044\000\000\000\044\000\000\000\000\000\044\000\044\000\040\000\
\000\000\040\000\000\000\000\000\040\000\040\000\000\000\000\000\
\000\000\000\000\000\000\044\000\044\000\044\000\044\000\044\000\
\044\000\107\000\040\000\040\000\000\000\000\000\070\000\071\000\
\072\000\073\000\000\000\000\000\000\000\074\000\075\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\083\000\084\000\
\109\000\000\000\000\000\000\000\070\000\071\000\072\000\073\000\
\000\000\000\000\000\000\074\000\075\000\076\000\077\000\078\000\
\079\000\080\000\081\000\082\000\083\000\084\000\070\000\071\000\
\072\000\073\000\000\000\000\000\000\000\074\000\075\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\083\000\084\000\
\070\000\071\000\072\000\073\000\000\000\000\000\000\000\074\000\
\075\000\000\000\000\000\078\000\079\000\080\000\081\000\082\000\
\083\000\084\000\070\000\071\000\072\000\073\000\000\000\000\000\
\000\000\074\000\075\000\000\000\000\000\000\000\000\000\000\000\
\000\000\082\000\083\000\084\000"

let yycheck = "\043\000\
\039\000\001\000\046\000\006\001\001\001\018\001\019\001\051\000\
\052\000\053\000\005\001\002\001\015\001\011\001\012\001\013\001\
\014\001\008\001\015\001\063\000\018\001\019\001\061\000\039\001\
\068\000\069\000\070\000\071\000\072\000\073\000\028\001\002\001\
\076\000\077\000\078\000\079\000\080\000\081\000\082\000\083\000\
\084\000\012\001\033\000\038\001\013\001\014\001\017\001\038\000\
\032\001\018\001\019\001\002\001\038\001\002\001\004\001\001\001\
\001\000\003\001\003\001\028\001\006\001\007\001\033\001\034\001\
\035\001\036\001\011\000\038\001\038\001\040\001\041\001\021\000\
\017\000\003\001\020\001\021\001\021\000\006\001\004\001\029\000\
\038\001\038\001\027\000\001\001\029\000\001\001\005\001\003\001\
\033\000\004\001\006\001\007\001\029\001\038\000\038\001\011\001\
\012\001\013\001\014\001\005\001\005\001\015\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\001\001\006\001\003\001\002\001\001\001\006\001\007\001\
\032\001\033\001\034\001\011\001\012\001\013\001\014\001\003\001\
\040\001\041\001\042\001\003\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\001\001\003\001\003\001\
\001\001\001\001\006\001\007\001\033\001\034\001\005\001\011\001\
\012\001\013\001\014\001\040\001\041\001\001\001\003\001\011\000\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\001\001\062\000\003\001\107\000\255\255\006\001\007\001\
\255\255\255\255\255\255\011\001\012\001\013\001\014\001\255\255\
\255\255\255\255\255\255\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\001\001\255\255\003\001\
\255\255\255\255\006\001\007\001\255\255\255\255\255\255\011\001\
\012\001\013\001\014\001\255\255\255\255\255\255\255\255\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\001\001\255\255\003\001\255\255\255\255\006\001\007\001\
\255\255\255\255\255\255\011\001\012\001\013\001\014\001\255\255\
\255\255\255\255\255\255\001\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\011\001\012\001\013\001\
\014\001\255\255\255\255\255\255\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\001\001\
\255\255\003\001\255\255\255\255\006\001\007\001\255\255\255\255\
\255\255\011\001\012\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\001\001\255\255\003\001\255\255\255\255\006\001\
\007\001\255\255\255\255\255\255\011\001\012\001\001\001\255\255\
\003\001\255\255\255\255\006\001\007\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\255\255\255\255\255\255\
\255\255\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\001\001\255\255\003\001\255\255\255\255\006\001\007\001\
\255\255\255\255\001\001\255\255\003\001\255\255\255\255\006\001\
\007\001\255\255\255\255\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\003\001\020\001\021\001\022\001\
\023\001\024\001\025\001\255\255\011\001\012\001\013\001\014\001\
\255\255\255\255\255\255\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\003\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\011\001\012\001\
\013\001\014\001\255\255\255\255\255\255\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\001\001\255\255\003\001\255\255\255\255\006\001\007\001\001\001\
\255\255\003\001\255\255\255\255\006\001\007\001\255\255\255\255\
\255\255\255\255\255\255\020\001\021\001\022\001\023\001\024\001\
\025\001\255\255\020\001\021\001\022\001\023\001\024\001\025\001\
\001\001\255\255\003\001\255\255\255\255\006\001\007\001\001\001\
\255\255\003\001\255\255\255\255\006\001\007\001\255\255\255\255\
\255\255\255\255\255\255\020\001\021\001\022\001\023\001\024\001\
\025\001\006\001\020\001\021\001\255\255\255\255\011\001\012\001\
\013\001\014\001\255\255\255\255\255\255\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\007\001\255\255\255\255\255\255\011\001\012\001\013\001\014\001\
\255\255\255\255\255\255\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\011\001\012\001\
\013\001\014\001\255\255\255\255\255\255\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\011\001\012\001\013\001\014\001\255\255\255\255\255\255\018\001\
\019\001\255\255\255\255\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\011\001\012\001\013\001\014\001\255\255\255\255\
\255\255\018\001\019\001\255\255\255\255\255\255\255\255\255\255\
\255\255\026\001\027\001\028\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  RBRAC\000\
  LBRAC\000\
  COLON\000\
  DOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  STAR\000\
  NOT\000\
  INC\000\
  DEC\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  OR\000\
  AND\000\
  MOD\000\
  RETURN\000\
  TRANS\000\
  KEYS\000\
  DFA\000\
  STACK\000\
  MAP\000\
  EOF\000\
  MAIN\000\
  STRING\000\
  INT\000\
  VOID\000\
  "

let yynames_block = "\
  INT_LITERAL\000\
  STRING_LITERAL\000\
  TYPE\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'main) in
    Obj.repr(
# 32 "parser.mly"
         ([],_1)
# 382 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'dfadecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.program) in
    Obj.repr(
# 33 "parser.mly"
                      ( (_1 :: fst _2), snd _2 )
# 390 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'param) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'node_list) in
    Obj.repr(
# 38 "parser.mly"
    ( { return = VOID;
    fname = "main";
    formals = _4;
    body = List.rev _7 :: List.rev _8 })
# 402 "parser.ml"
               : 'main))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
          (Int)
# 408 "parser.ml"
               : 'var_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
          (String)
# 414 "parser.ml"
               : 'var_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                (Stack)
# 420 "parser.ml"
               : 'var_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                (Map)
# 426 "parser.ml"
               : 'var_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var_type) in
    Obj.repr(
# 50 "parser.mly"
             (Datatype(_1))
# 433 "parser.ml"
               : 'ret_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
           (Datatype(Void))
# 439 "parser.ml"
               : 'ret_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : 'ret_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'node_list) in
    Obj.repr(
# 55 "parser.mly"
    ( { return = _1;
    fname = Ident(_3);
    formals = _5;
    body = List.rev _8 :: List.rev _9 })
# 453 "parser.ml"
               : 'dfadecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
    ([])
# 459 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 63 "parser.mly"
                            ((*A MAGICAL LIST OF VDECLS*))
# 467 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                  ( (*declare id*) )
# 475 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'var_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'id_list) in
    Obj.repr(
# 67 "parser.mly"
                                ( (* declare multiple id's*) )
# 484 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'var_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                              ((*assign expr to id*) )
# 493 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'var_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'id_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                                            ((*Assign several variables to a single expr*))
# 503 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 72 "parser.mly"
         ((*for one ID*))
# 510 "parser.ml"
               : 'id_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'id_list) in
    Obj.repr(
# 73 "parser.mly"
                       ((*keep getting more ID's*))
# 518 "parser.ml"
               : 'id_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
    ([])
# 524 "parser.ml"
               : 'node_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'meta_node_block) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'node_list) in
    Obj.repr(
# 77 "parser.mly"
                                                 ((*A list of nodes*))
# 533 "parser.ml"
               : 'node_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'return_stmt) in
    Obj.repr(
# 81 "parser.mly"
             ()
# 540 "parser.ml"
               : 'meta_node_block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                      ( (*return expression*))
# 547 "parser.ml"
               : 'return_stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
    ([])
# 553 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 90 "parser.mly"
                  ( List.rev _1)
# 560 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 93 "parser.mly"
          ( [_1] )
# 567 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 94 "parser.mly"
                              ( _3 :: _1)
# 575 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "parser.mly"
                  ( Formal(Datatype(_1),Ident(_2)) )
# 583 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
    ([])
# 589 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 101 "parser.mly"
                           ( _1 :: _3 )
# 597 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
           ( [_1] )
# 604 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 105 "parser.mly"
                   ( IntLit(_1)   )
# 611 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 106 "parser.mly"
                   ( StringLit(_1))
# 618 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'var_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                                ( Cast(Datatype(_1),_3))
# 626 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 108 "parser.mly"
                     ( Variable(Ident(_1))  )
# 633 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 641 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 649 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                     ( Binop(_1, Mult,  _3) )
# 657 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 665 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 673 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 681 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 689 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 697 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                     ( Binop(_1, Greater,_3))
# 705 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 713 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                     ( Binop(_1, Mod,   _3) )
# 721 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                     ( Binop(_1, And,   _3) )
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                     ( Binop(_1, Or ,   _3) )
# 737 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                            ( (*Unop(PostInc, $1)*) )
# 744 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                            ( (*Unop(PostDec, $1)*) )
# 751 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                            ( Unop(Neg, _2) )
# 758 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                            ( Unop(Not, _2) )
# 765 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                                            ( (*Maps get element*) )
# 773 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                       ( _2 )
# 780 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 130 "parser.mly"
                                            ((*call a sub dfa*))
# 788 "parser.ml"
               : 'expr))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
