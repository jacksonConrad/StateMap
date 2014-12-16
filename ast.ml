type var_type = Int | String | Stack | Float | Void | Eos

type binop = Add | Sub | Mult | Div | Mod | Equal | Neq | And | Or| Lt | Leq | Gt | Geq
type unop = Not | Neg

type ident =
    Ident of string

type datatype = 
    Datatype of var_type |
    Stacktype of datatype|
    Eostype of var_type

type expr = 
    IntLit of int | 
    StringLit of string |
    FloatLit of float  |
    EosLit |
    Variable of ident |
    Unop of unop * expr |
    Binop of expr * binop * expr |
    Call of ident * expr list |
    Push of ident * expr |
    Pop of ident |
    Peek of ident 

type value = 
    ExprVal of expr

and decl = 
    VarDecl of datatype * ident |
    VarAssignDecl of datatype * ident * value

type stmt = 
    Block of stmt list |
    Expr of expr | 
    Declaration of decl |
    Assign of ident * expr |
    Transition of ident * expr |
    Return of expr 

type formal = 
    Formal of datatype * ident

type node = 
    Node of ident * stmt list

type dfa_decl = {
    return : datatype;
    dfa_name: ident;
    formals : formal list;
    var_body : decl list;
    node_body : node list;
}


type program = dfa_decl list

(* "Pretty printed" version of the AST, meant to generate a MicroC program
   from the AST.  These functions are only for pretty-printing (the -a flag)
   the AST and can be removed. *)
let string_of_ident = function
    Ident(l) -> l

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | StringLit(l) -> l
  | FloatLit(l) -> string_of_float l
  | Variable(id) -> string_of_ident id 
  | Unop(o, e) -> 
      string_of_expr e ^ " " ^ 
      (match o with
        Not -> "!"  |
        Neg -> "-")
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
      Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!=" | Mod -> "%"
      | Lt -> "<" | Leq -> "<=" | Gt -> ">" | Geq -> ">=" | And -> "&&" | Or -> "||" ) ^ " " ^
      string_of_expr e2
  | Call(id, e_list) -> string_of_ident id ^ " " ^ 
      "(" ^ String.concat ", " (List.map string_of_expr e_list) ^ ")"
  | Push(id, e) -> string_of_ident id ^ " " ^ string_of_expr e 
  | Pop(id) -> string_of_ident id
  | Peek(id) -> string_of_ident id
  | EosLit -> "EOSLIT"

let rec string_of_datatype = function
  Datatype(vartype) -> 
    (match vartype with 
      Int -> "int" | String -> "String" | Stack -> "Stack" | Float -> "Float"
      | Void -> "Void" | Eos -> "Eos"
    )
  | Stacktype(datatype) -> "Stack<" ^ string_of_datatype datatype ^ ">"
  | Eostype(_) -> "EOS"



let string_of_decl = function
    VarDecl(dt, id) -> string_of_datatype dt ^ " " ^ string_of_ident id
  | VarAssignDecl(dt,id,value) -> string_of_datatype dt ^ " " ^ string_of_ident id
      ^ " = " ^ (match value with 
                ExprVal(e) -> string_of_expr e)

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | Assign(id, expr) -> string_of_ident id ^ " = " ^ string_of_expr expr ^
  ";\n"
  | Declaration(decl) -> string_of_decl decl
  | Transition(id, expr) -> string_of_ident id ^ " <- (" ^ string_of_expr expr ^ ")"

let string_of_node = function
  Node(id, stmtlist) -> string_of_ident id ^ " {\n" ^
    String.concat "\n" (List.map string_of_stmt stmtlist) ^ "\n}"

let string_of_formal = function
  Formal(dt, id) -> string_of_datatype dt ^ " " ^ string_of_ident id

let string_of_dfadecl dfadecl =
  string_of_datatype dfadecl.return ^ " " ^ string_of_ident dfadecl.dfa_name ^ "(" ^ String.concat ", " (List.map string_of_formal dfadecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_decl dfadecl.var_body) ^
  String.concat "" (List.map string_of_node dfadecl.node_body) ^
  "}\n"

let string_of_program (program) =
  String.concat "" (List.map string_of_dfadecl program)

