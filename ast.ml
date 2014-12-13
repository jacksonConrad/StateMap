type var_type = Int | String | Stack | Float | Void | Eos

type binop = Add | Sub | Mult | Div | Mod | Equal | Neq | And | Or| Lt | Leq | Gt | Geq
type unop = Not | Neg

type ident =
    Ident of string

type datatype = 
    Datatype of var_type | (* Possible type for concurrent DFA return stack*)
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
    (* ExprAssign of ident * expr | *)
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
    (*TransWrapper of expr | Why do we have this????*)
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

(* Low-level AST printing, to help debug the structure.  These functions are
   only for debugging (the -r flag) and can be removed. *)

(* 
let rec expr_s = function
   IntLit(l) -> "IntLit" ^ string_of_int l
 | StringLit(l) -> "StringLit" ^ l
 | Variable(l) -> "Variable " ^ l
 | Id(s) -> "Id " ^ s
 | Binop(e1, o, e2) -> "Binop (" ^ expr_s e1 ^ ") " ^
       (match o with Add -> "Add" | Sub -> "Sub" | Mult -> "Mult" |
                     Div -> "Div" | Equal -> "Equal" | Neq -> "Neq" |
                     Less -> "Less" | Leq -> "Leq" | Greater -> "Greater" |
                     Geq -> "Geq") ^ " (" ^ expr_s e2 ^ ")"
 | Assign(v, e) -> "Assign " ^ v ^ " (" ^ expr_s e ^ ")"
 | Call(f, es) -> "Call " ^ f ^ " [" ^
        String.concat ", " (List.map (fun e -> "(" ^ expr_s e ^ ")") es) ^ "]"
 | Noexpr -> "Noexpr"


let rec stmt_s = function
   Block(ss) -> "Block [" ^ String.concat ",\n"
                             (List.map (fun s -> "(" ^ stmt_s s ^ ")") ss) ^ "]"
 | Expr(e) -> "Expr (" ^ expr_s e ^ ")"
 | Return(e) -> "Return (" ^ expr_s e ^ ")"
 | If(e, s1, s2) -> "If (" ^ expr_s e ^ ") (" ^ stmt_s s1 ^ ") (" ^
                                                stmt_s s2 ^ ")"
 | For(e1, e2, e3, s) -> "For (" ^ expr_s e1 ^ ") (" ^ expr_s e2 ^
                            ") (" ^ expr_s e3 ^ ") (" ^ stmt_s s ^ ")"
 | While(e, s) -> "While (" ^ expr_s e ^ ") (" ^ stmt_s s ^ ")"

let func_decl_s f =
  " { dfa_name = \"" ^ f.dfa_name ^ "\"\n   formals = [" ^
  String.concat ", " f.formals ^ "]\n   locals = [" ^
  String.concat ", " f.locals ^ "]\n   body = ["  ^
  String.concat ",\n" (List.map stmt_s f.body) ^
  "]}\n"

let program_s (vars, funcs) = "([" ^ String.concat ", " vars ^ "],\n" ^
  String.concat "\n" (List.map func_decl_s funcs) ^ ")" *)

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
  (* | ExprAssign(id, e) -> string_of_ident id ^ " " ^ string_of_expr e *)
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
  ";\n";
  (* | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s *)
  (* | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^ *)
      (* string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2 *)
  (* | For(e1, e2, e3, s) -> *)
      (* "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ *)
      (* string_of_expr e3  ^ ") " ^ string_of_stmt s *)
  (* | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s *)
  | Declaration(decl) -> string_of_decl decl
  | Transition(id, expr) -> string_of_ident id ^ " <- (" ^ string_of_expr expr ^ ")"




let string_of_node = function
  Node(id, stmtlist) -> string_of_ident id ^ " {\n" ^
    String.concat "\n" (List.map string_of_stmt stmtlist) ^ "\n}"

let string_of_formal = function
  Formal(dt, id) -> string_of_datatype dt ^ " " ^ string_of_ident id



(* let string_of_vdecl id = "int " ^ id ^ ";\n" *)

let string_of_dfadecl dfadecl =
  string_of_datatype dfadecl.return ^ " " ^ string_of_ident dfadecl.dfa_name ^ "(" ^ String.concat ", " (List.map string_of_formal dfadecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_decl dfadecl.var_body) ^
  String.concat "" (List.map string_of_node dfadecl.node_body) ^
  "}\n"

let string_of_program (program) =
  String.concat "" (List.map string_of_dfadecl program)

