%{ open Ast %}
%{ open Type %}
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA RBRAC LBRAC COLON DOT
%token PLUS MINUS TIMES DIVIDE ASSIGN STAR
%token NOT INC DEC
%token EQ NEQ LT LEQ GT GEQ OR AND MOD
%token RETURN TRANS
%token KEYS DFA STACK MAP
/* %token PUT PUSH POP PEEK SIZE CONTAINS DEL */
%token <int> INT_LITERAL
%token <string> STRING_LITERAL TYPE ID
%token EOF
%token MAIN
%token STRING INT VOID

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
    main {[],$1}
    | dfadecl program { ($1 :: fst $2), snd $2 }


main:
    DFA MAIN LPAREN formals_opt RPAREN LBRACE vdecl_list node_list RBRACE
    { { return = VOID;
    fname = "main";
    formals = $4;
    body = List.rev $7 :: List.rev $8 }}
    
var_type:
	  INT   	{Int}
	|STRING		{String}
    |STACK      {Stack}
    |MAP        {Map}

ret_type:
    var_type {Datatype($1)}
    | VOID {Datatype(Void)}

dfadecl:
    ret_type DFA ID LPAREN formals_opt RPAREN LBRACE vdecl_list node_list RBRACE
    { { return = $1;
    fname = Ident($3);
    formals = $5;
    body = List.rev $8 :: List.rev $9 }} 

vdecl_list:
    {[]}
    | vdecl SEMI vdecl_list {(*A MAGICAL LIST OF VDECLS*)}

vdecl:
      var_type ID { (*declare id*) }
    | var_type ID COMMA id_list { (* declare multiple id's*) }
    | var_type ID ASSIGN expr {(*assign expr to id*) }
    | var_type ID COMMA id_list ASSIGN expr {(*Assign several variables to a single expr*)}

id_list:
      ID {(*for one ID*)}
    | ID COMMA id_list {(*keep getting more ID's*)}

node_list:/*TODO come back here and think about START */
    {[]}
    | ID LBRACE meta_node_block RBRACE node_list {(*A list of nodes*)}

meta_node_block: /*Enforces trans vs return node types*/
      trans_node_list   {(*If you have a trans node, do trans_node_list*)}
    | node_stmt_list return_stmt {(*Else, you have a return node*)}

trans_node_list: /*For more transitions*/
      ID TRANS STAR SEMI  {(*Force a catch-all transition at the end*)}
    | node_stmt_list trans_stmt trans_node_list {(*do none or more stuff, then a transition, then repeat*)}

trans_stmt:
    ID TRANS expr SEMI { (*Transition to state if true*)}

return_stmt:
    RETURN expr SEMI  { (*return expression*)}

node_stmt_list:
    {[]}
    | node_stmt node_stmt_list {(*A MAGICAL LIST OF NODE STMTS*)}

node_stmt:
      expr  SEMI { Expr($1)    }
    | vdecl SEMI { (*Declare*) }
    | ID LBRAC expr RBRAC ASSIGN expr SEMI {(*Assign value to map:essentially,map.push(key,val)*)}
    | ID ASSIGN expr SEMI {(*Assign value to variable*)}

formals_opt:
    {[]} /*nothing*/
    | formal_list { List.rev $1}

formal_list:
    param { [$1] }
    | formal_list COMMA param { $3 :: $1}

param:
      var_type ID { Formal(Datatype($1),Ident($2)) }
    | var_type ID LT TYPE COMMA TYPE GT {(*FOR DECLARING A MAP*)} 

expr_list:
    {[]}
    | expr COMMA expr_list { $1 :: $3 }
    | expr { [$1] }

expr:
    INT_LITERAL    { IntLit($1)   }
  | STRING_LITERAL { StringLit($1)}
  | var_type LPAREN expr RPAREN { Cast(Datatype($1),$3)}
  | ID               { Variable(Ident($1))  }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,$3)}
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr MOD    expr { Binop($1, Mod,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or ,   $3) }
  | expr INC                { (*Unop(PostInc, $1)*) }
  | expr DEC                { (*Unop(PostDec, $1)*) }
  /*| INC expr                { (*Unop(PreInc, $2)*)}
  | DEC expr                { (*Unop(PreDec, $2)*)} TODO FIGURE OUT HOW TO INCLUDE BOTH PRE AND POST FIX OPERATORS*/
  | MINUS expr %prec UMINUS { Unop(Neg, $2) }
  | NOT   expr              { Unop(Not, $2) }
  | ID LBRAC expr RBRAC                     { (*Maps get element*) }
  | ID DOT CONTAINS LPAREN expr RPAREN      { (*isMember function*)}
  | ID DOT PUSH LPAREN expr RPAREN          { (*Push expr into*)   }
  | ID DOT PUSH LPAREN expr COMMA expr RPAREN{(*Push into a map?*) }
  | ID DOT POP                              { (*Pop from stack*)   }
  | ID DOT DEL LPAREN expr RPAREN           { (*Delete a key from map*) }
  | ID DOT PEEK                             { (*Peek key from stack*) }
  | ID DOT SIZE                             { (*Get size of stack/Map (# of keys)*) }
  | ID DOT KEYS                             { (*Get a ??STACK?? of keys? *) }
  | LPAREN expr RPAREN { $2 }
  | ID LPAREN expr_list RPAREN              {(*call a sub dfa*)}/*THIS IS A SHIFT/REDUCE ERROR WITH line 152 and line 123 maybe???*/
