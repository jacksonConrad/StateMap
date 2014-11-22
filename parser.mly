%{ open Ast %}
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA RBRAC LBRAC COLON DOT
%token PLUS MINUS TIMES DIVIDE ASSIGN STAR
%token NOT INC DEC
%token EQ NEQ LT LEQ GT GEQ OR AND MOD
%token RETURN TRANS
%token DFA STACK
%token <int> INT_LITERAL
%token <string> STRING_LITERAL TYPE ID
%token EOF
%token MAIN
%token STRING INT VOID DOUBLE

%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left AND OR
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT
%right UMINUS DEC INC /*TODO HOW TO MAKE DEC/INC BIND LEFT AND RIGHT!? AND IN WHAT PRECEDENCE???*/
%nonassoc LPAREN RPAREN LBRAC RBRAC

%start program
%type <Ast.program> program

%%

program:
    dfa_decl_list {$1}

var_type:
     INT    {Int}
    |STRING {String}
    |STACK  {Stack}
    |DOUBLE {Double}
    |VOID   {Void}

ret_type:
    var_type {Datatype($1)}

dfa_decl:
    ret_type DFA ID LPAREN formals_opt RPAREN LBRACE vdecl_list node_list RBRACE
    { { return = $1;
    fname = Ident($3);
    formals = $5;
    var_body = $8; 
    node_body = $9}} 

dfa_decl_list:
  {[]}
  | dfa_decl dfa_decl_list { $1 :: $2 }


vdecl_list:
    {[]}
    | vdecl vdecl_list { $1 :: $2 } 

vdecl:
      var_type ID SEMI { VarDecl(Datatype($1), Ident($2)) }
    | var_type ID ASSIGN expr SEMI { VarAssignDecl(Datatype($1), Ident($2), ExprVal($4))}

node_list:
    {[]}
    | node node_list { $1 :: $2 }

node:
  ID LBRACE stmt_list RBRACE { Node(Ident($1), $3) }

stmt_list:
	{[]}
	| stmt stmt_list { $1 :: $2 }

/* TODO: add method calls */
stmt:
	RETURN expr SEMI  {Return($2)}
	| ID TRANS expr SEMI {Transition(Ident($1),$3)} 
	| ID TRANS STAR SEMI {Transition(Ident($1),IntLit(1))} /*Note expr = 1 here since eval( * )==TRUE*/
	| vdecl {Declaration($1)}
	| expr SEMI {Expr($1)}

formals_opt:
    {[]} /*nothing*/
    | formal_list { List.rev $1}

formal_list:
    param { [$1] }
    | formal_list COMMA param { $3 :: $1}

param:
      var_type ID { Formal(Datatype($1),Ident($2)) }

expr_list:
    {[]}
    | expr COMMA expr_list { $1 :: $3 }
    | expr { [$1] }

expr:
    INT_LITERAL    { IntLit($1)   }
  | STRING_LITERAL { StringLit($1)}
  /*| var_type LPAREN expr RPAREN { Cast(Datatype($1),$3)}*/
  | ID               { Variable(Ident($1))  }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Lt,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Gt,$3)}
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr MOD    expr { Binop($1, Mod,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or ,   $3) }
  | INC expr         { Unop(Inc, $2) (*Unop(PreInc, $1)*) }
  | DEC expr         { Unop(Dec, $2) (*Unop(PreDec, $1)*) }
  | MINUS expr %prec UMINUS { Unop(Neg, $2) }
  | NOT   expr              { Unop(Not, $2) }
  | LPAREN expr RPAREN { $2 }
  | ID LPAREN expr_list RPAREN {Call(Ident($1), $3) (*call a sub dfa*)}
