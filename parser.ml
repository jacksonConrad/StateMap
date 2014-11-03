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
  | DFA
  | STACK
  | INT_LITERAL of (int)
  | STRING_LITERAL of (string)
  | TYPE of (string)
  | ID of (string)
  | EOF
  | MAIN
  | STRING
  | INT
  | VOID
  | DOUBLE

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 50 "parser.ml"
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
  287 (* DFA *);
  288 (* STACK *);
    0 (* EOF *);
  293 (* MAIN *);
  294 (* STRING *);
  295 (* INT *);
  296 (* VOID *);
  297 (* DOUBLE *);
    0|]

let yytransl_block = [|
  289 (* INT_LITERAL *);
  290 (* STRING_LITERAL *);
  291 (* TYPE *);
  292 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\007\000\007\000\007\000\007\000\008\000\
\008\000\003\000\005\000\005\000\010\000\010\000\010\000\010\000\
\011\000\011\000\006\000\006\000\013\000\013\000\014\000\014\000\
\014\000\014\000\014\000\009\000\009\000\015\000\015\000\004\000\
\016\000\016\000\016\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\000\000"

let yylen = "\002\000\
\001\000\002\000\009\000\001\000\001\000\001\000\001\000\001\000\
\001\000\010\000\000\000\002\000\003\000\005\000\005\000\007\000\
\001\000\003\000\000\000\005\000\000\000\002\000\003\000\004\000\
\004\000\001\000\002\000\000\000\001\000\001\000\003\000\002\000\
\000\000\003\000\001\000\001\000\001\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\002\000\002\000\002\000\003\000\
\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\006\000\005\000\004\000\009\000\007\000\
\058\000\001\000\000\000\008\000\000\000\000\000\002\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\032\000\030\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\031\000\000\000\000\000\000\000\012\000\000\000\000\000\
\003\000\013\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\036\000\037\000\000\000\026\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\010\000\000\000\
\054\000\055\000\052\000\053\000\000\000\000\000\000\000\027\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\022\000\000\000\
\014\000\000\000\015\000\056\000\023\000\000\000\000\000\000\000\
\000\000\000\000\000\000\041\000\042\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\049\000\020\000\018\000\
\000\000\000\000\057\000\025\000\024\000\016\000\034\000"

let yydgoto = "\002\000\
\009\000\010\000\011\000\019\000\030\000\036\000\031\000\013\000\
\025\000\032\000\060\000\056\000\057\000\058\000\026\000\095\000"

let yysindex = "\016\000\
\059\255\000\000\240\254\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\059\255\000\000\243\254\022\255\000\000\011\255\
\237\254\053\255\047\255\020\255\237\254\056\255\000\000\000\000\
\061\255\065\255\237\254\058\255\237\254\044\255\052\255\237\254\
\237\254\000\000\080\255\090\255\048\255\000\000\044\255\040\255\
\000\000\000\000\067\255\075\255\096\255\075\255\075\255\075\255\
\075\255\075\255\075\255\000\000\000\000\014\255\000\000\122\255\
\099\255\040\255\106\255\010\255\111\255\140\255\000\000\086\000\
\000\000\000\000\000\000\000\000\158\255\075\255\049\255\000\000\
\075\255\075\255\075\255\075\255\075\255\075\255\075\255\075\255\
\075\255\075\255\075\255\075\255\075\255\044\255\000\000\067\255\
\000\000\075\255\000\000\000\000\000\000\146\000\116\255\119\255\
\176\255\251\254\251\254\000\000\000\000\179\000\179\000\198\255\
\198\255\198\255\198\255\227\255\227\255\000\000\000\000\000\000\
\194\255\075\255\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\061\255\000\000\000\000\000\000\
\000\000\118\255\007\255\000\000\000\000\117\255\000\000\007\255\
\007\255\000\000\000\000\000\000\000\000\000\000\117\255\132\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\222\255\000\000\000\000\
\000\000\132\255\074\255\000\000\104\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\135\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\117\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\136\255\000\000\000\000\
\000\000\250\255\021\000\000\000\000\000\174\000\178\000\070\000\
\114\000\120\000\126\000\033\000\060\000\000\000\000\000\000\000\
\000\000\135\255\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\129\000\000\000\000\000\024\000\238\255\218\255\085\000\000\000\
\000\000\226\255\067\000\212\255\098\000\000\000\000\000\043\000"

let yytablesize = 463
let yytable = "\062\000\
\045\000\064\000\065\000\066\000\067\000\068\000\069\000\075\000\
\076\000\055\000\089\000\011\000\004\000\038\000\039\000\070\000\
\001\000\016\000\005\000\006\000\014\000\008\000\085\000\017\000\
\090\000\094\000\097\000\055\000\098\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\107\000\108\000\109\000\
\110\000\046\000\011\000\071\000\024\000\113\000\018\000\111\000\
\042\000\022\000\046\000\047\000\034\000\043\000\021\000\023\000\
\048\000\049\000\050\000\027\000\047\000\033\000\044\000\028\000\
\096\000\048\000\049\000\050\000\051\000\094\000\029\000\004\000\
\052\000\053\000\017\000\054\000\046\000\005\000\006\000\035\000\
\008\000\052\000\053\000\040\000\061\000\012\000\047\000\037\000\
\017\000\003\000\004\000\048\000\049\000\050\000\041\000\012\000\
\005\000\006\000\007\000\008\000\063\000\020\000\059\000\086\000\
\038\000\020\000\038\000\052\000\053\000\038\000\061\000\088\000\
\070\000\020\000\038\000\038\000\038\000\038\000\115\000\116\000\
\029\000\019\000\072\000\038\000\038\000\038\000\038\000\038\000\
\038\000\038\000\038\000\038\000\073\000\074\000\075\000\076\000\
\021\000\033\000\035\000\015\000\091\000\077\000\078\000\079\000\
\080\000\081\000\082\000\083\000\084\000\085\000\073\000\074\000\
\075\000\076\000\112\000\087\000\119\000\000\000\093\000\077\000\
\078\000\079\000\080\000\081\000\082\000\083\000\084\000\085\000\
\073\000\074\000\075\000\076\000\000\000\000\000\000\000\000\000\
\117\000\077\000\078\000\079\000\080\000\081\000\082\000\083\000\
\084\000\085\000\073\000\074\000\075\000\076\000\000\000\000\000\
\000\000\000\000\118\000\077\000\078\000\079\000\080\000\081\000\
\082\000\083\000\084\000\085\000\073\000\074\000\075\000\076\000\
\073\000\074\000\075\000\076\000\000\000\077\000\078\000\079\000\
\080\000\081\000\082\000\083\000\084\000\085\000\038\000\083\000\
\084\000\085\000\000\000\000\000\000\000\000\000\000\000\000\000\
\038\000\038\000\038\000\038\000\000\000\073\000\074\000\075\000\
\076\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
\038\000\038\000\039\000\000\000\039\000\000\000\085\000\039\000\
\000\000\000\000\000\000\000\000\039\000\039\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\039\000\039\000\039\000\
\039\000\039\000\039\000\039\000\039\000\040\000\000\000\040\000\
\000\000\000\000\040\000\000\000\000\000\000\000\000\000\040\000\
\040\000\051\000\000\000\051\000\000\000\000\000\051\000\000\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\000\000\000\000\000\000\000\000\051\000\051\000\051\000\051\000\
\051\000\051\000\051\000\051\000\050\000\000\000\050\000\000\000\
\000\000\050\000\000\000\000\000\000\000\000\000\045\000\000\000\
\045\000\000\000\000\000\045\000\000\000\000\000\000\000\050\000\
\050\000\050\000\050\000\050\000\050\000\050\000\050\000\000\000\
\092\000\045\000\045\000\045\000\045\000\045\000\045\000\000\000\
\073\000\074\000\075\000\076\000\000\000\000\000\000\000\000\000\
\000\000\077\000\078\000\079\000\080\000\081\000\082\000\083\000\
\084\000\085\000\046\000\000\000\046\000\000\000\000\000\046\000\
\047\000\000\000\047\000\000\000\000\000\047\000\048\000\000\000\
\048\000\000\000\000\000\048\000\000\000\046\000\046\000\046\000\
\046\000\046\000\046\000\047\000\047\000\047\000\047\000\047\000\
\047\000\048\000\048\000\048\000\048\000\048\000\048\000\114\000\
\000\000\000\000\000\000\000\000\073\000\074\000\075\000\076\000\
\000\000\000\000\000\000\000\000\000\000\077\000\078\000\079\000\
\080\000\081\000\082\000\083\000\084\000\085\000\043\000\000\000\
\043\000\000\000\044\000\043\000\044\000\000\000\000\000\044\000\
\000\000\000\000\000\000\000\000\000\000\073\000\074\000\075\000\
\076\000\043\000\043\000\000\000\000\000\044\000\044\000\000\000\
\079\000\080\000\081\000\082\000\083\000\084\000\085\000"

let yycheck = "\044\000\
\039\000\046\000\047\000\048\000\049\000\050\000\051\000\013\001\
\014\001\040\000\001\001\005\001\032\001\032\000\033\000\002\001\
\001\000\031\001\038\001\039\001\037\001\041\001\028\001\002\001\
\015\001\070\000\071\000\058\000\073\000\074\000\075\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\002\001\036\001\030\001\021\000\090\000\036\001\086\000\
\001\001\003\001\002\001\012\001\029\000\006\001\002\001\036\001\
\017\001\018\001\019\001\004\001\012\001\004\001\015\001\003\001\
\016\001\017\001\018\001\019\001\029\001\114\000\006\001\032\001\
\033\001\034\001\001\001\036\001\002\001\038\001\039\001\036\001\
\041\001\033\001\034\001\004\001\036\001\001\000\012\001\036\001\
\015\001\031\001\032\001\017\001\018\001\019\001\005\001\011\000\
\038\001\039\001\040\001\041\001\005\001\017\000\036\001\005\001\
\001\001\021\000\003\001\033\001\034\001\006\001\036\001\006\001\
\002\001\029\000\011\001\012\001\013\001\014\001\003\001\001\001\
\003\001\005\001\001\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\011\001\012\001\013\001\014\001\
\005\001\003\001\003\001\011\000\001\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\011\001\012\001\
\013\001\014\001\088\000\058\000\114\000\255\255\001\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\011\001\012\001\013\001\014\001\255\255\255\255\255\255\255\255\
\001\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\011\001\012\001\013\001\014\001\255\255\255\255\
\255\255\255\255\001\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\011\001\012\001\013\001\014\001\
\011\001\012\001\013\001\014\001\255\255\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\001\001\026\001\
\027\001\028\001\255\255\255\255\255\255\255\255\255\255\255\255\
\011\001\012\001\013\001\014\001\255\255\011\001\012\001\013\001\
\014\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\001\001\255\255\003\001\255\255\028\001\006\001\
\255\255\255\255\255\255\255\255\011\001\012\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\001\001\255\255\003\001\
\255\255\255\255\006\001\255\255\255\255\255\255\255\255\011\001\
\012\001\001\001\255\255\003\001\255\255\255\255\006\001\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\255\255\255\255\255\255\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\001\001\255\255\003\001\255\255\
\255\255\006\001\255\255\255\255\255\255\255\255\001\001\255\255\
\003\001\255\255\255\255\006\001\255\255\255\255\255\255\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\255\255\
\003\001\020\001\021\001\022\001\023\001\024\001\025\001\255\255\
\011\001\012\001\013\001\014\001\255\255\255\255\255\255\255\255\
\255\255\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\001\001\255\255\003\001\255\255\255\255\006\001\
\001\001\255\255\003\001\255\255\255\255\006\001\001\001\255\255\
\003\001\255\255\255\255\006\001\255\255\020\001\021\001\022\001\
\023\001\024\001\025\001\020\001\021\001\022\001\023\001\024\001\
\025\001\020\001\021\001\022\001\023\001\024\001\025\001\006\001\
\255\255\255\255\255\255\255\255\011\001\012\001\013\001\014\001\
\255\255\255\255\255\255\255\255\255\255\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\001\001\255\255\
\003\001\255\255\001\001\006\001\003\001\255\255\255\255\006\001\
\255\255\255\255\255\255\255\255\255\255\011\001\012\001\013\001\
\014\001\020\001\021\001\255\255\255\255\020\001\021\001\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001"

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
  DFA\000\
  STACK\000\
  EOF\000\
  MAIN\000\
  STRING\000\
  INT\000\
  VOID\000\
  DOUBLE\000\
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
# 30 "parser.mly"
         ([],_1)
# 354 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'dfa_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.program) in
    Obj.repr(
# 31 "parser.mly"
                       ( (_1 :: fst _2), snd _2 )
# 362 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'param) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'node_list) in
    Obj.repr(
# 36 "parser.mly"
    ( { return = VOID;
    fname = "main";
    formals = _4;
    body = List.rev _7 :: List.rev _8 })
# 374 "parser.ml"
               : 'main))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
          (Int)
# 380 "parser.ml"
               : 'var_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "parser.mly"
          (String)
# 386 "parser.ml"
               : 'var_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                  (Stack)
# 392 "parser.ml"
               : 'var_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
                     (Double)
# 398 "parser.ml"
               : 'var_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var_type) in
    Obj.repr(
# 48 "parser.mly"
             (Datatype(_1))
# 405 "parser.ml"
               : 'ret_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
           (Datatype(Void))
# 411 "parser.ml"
               : 'ret_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : 'ret_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'node_list) in
    Obj.repr(
# 53 "parser.mly"
    ( { return = _1;
    fname = Ident(_3);
    formals = _5;
    body = List.rev _8 :: List.rev _9 })
# 425 "parser.ml"
               : 'dfa_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
    ([])
# 431 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 61 "parser.mly"
                       ((*A MAGICAL LIST OF VDECLS*))
# 439 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 64 "parser.mly"
                       ( (*declare id*) )
# 447 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'var_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'id_list) in
    Obj.repr(
# 65 "parser.mly"
                                     ( (* declare multiple id's*) )
# 456 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'var_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                                   ((*assign expr to id*) )
# 465 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'var_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'id_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                                                 ((*Assign several variables to a single expr*))
# 475 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "parser.mly"
         ((*for one ID*))
# 482 "parser.ml"
               : 'id_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'id_list) in
    Obj.repr(
# 71 "parser.mly"
                       ((*keep getting more ID's*))
# 490 "parser.ml"
               : 'id_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
    ([])
# 496 "parser.ml"
               : 'node_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'node_list) in
    Obj.repr(
# 75 "parser.mly"
                                           ((*A list of nodes*))
# 505 "parser.ml"
               : 'node_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
 ([])
# 511 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 79 "parser.mly"
                  ()
# 519 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                   (Return(_2))
# 526 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                      (Transition(_1,_3))
# 534 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 85 "parser.mly"
                      (Transition(_1,1))
# 541 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 86 "parser.mly"
         (Declaration(_1))
# 548 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
             (Expr(_1))
# 555 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
    ([])
# 561 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 91 "parser.mly"
                  ( List.rev _1)
# 568 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 94 "parser.mly"
          ( [_1] )
# 575 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 95 "parser.mly"
                              ( _3 :: _1)
# 583 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 98 "parser.mly"
                  ( Formal(Datatype(_1),Ident(_2)) )
# 591 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
    ([])
# 597 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 102 "parser.mly"
                           ( _1 :: _3 )
# 605 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
           ( [_1] )
# 612 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 106 "parser.mly"
                   ( IntLit(_1)   )
# 619 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 107 "parser.mly"
                   ( StringLit(_1))
# 626 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "parser.mly"
                     ( Variable(Ident(_1))  )
# 633 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 641 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 649 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                     ( Binop(_1, Mult,  _3) )
# 657 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 665 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 673 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 681 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 689 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 697 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                     ( Binop(_1, Greater,_3))
# 705 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 713 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                     ( Binop(_1, Mod,   _3) )
# 721 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                     ( Binop(_1, And,   _3) )
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                     ( Binop(_1, Or ,   _3) )
# 737 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                     ( Unop(Inc, _2) (*Unop(PreInc, $1)*) )
# 744 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                     ( Unop(Dec, _2) (*Unop(PreDec, $1)*) )
# 751 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                            ( Unop(Neg, _2) )
# 758 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                            ( Unop(Not, _2) )
# 765 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                       ( _2 )
# 772 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 128 "parser.mly"
                                            (Call(_1, _3) (*call a sub dfa*))
# 780 "parser.ml"
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
