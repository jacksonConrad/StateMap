type var_type = Int | String | Stack | Double | Void

type binop = Add | Sub | Mult | Div | Mod | Equal | Neq | And | Or| Lt | Leq | Gt | Geq
type unop = Inc | Dec | Not | Neg

type ident =
    Ident of string

type datatype = 
    Datatype of var_type | (* Possible type for concurrent DFA return stack*)
    Stacktype of datatype

type expr = 
    IntLit of int | 
    StringLit of string |
    EosLit |
    Variable of ident |
    Unop of unop * expr |
    Binop of expr * binop * expr |
    ExprAssign of ident * expr |
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
    fname: ident;
    formals : formal list;
    var_body : decl list;
    node_body : node list;
}


type program = dfa_decl list

(* Low-level AST printing, to help debug the structure.  These functions are
   only for debugging (the -r flag) and can be removed. *)


let rec expr_s = function
   Literal(l) -> "Literal " ^ string_of_int l
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
  " { fname = \"" ^ f.fname ^ "\"\n   formals = [" ^
  String.concat ", " f.formals ^ "]\n   locals = [" ^
  String.concat ", " f.locals ^ "]\n   body = ["  ^
  String.concat ",\n" (List.map stmt_s f.body) ^
  "]}\n"

let program_s (vars, funcs) = "([" ^ String.concat ", " vars ^ "],\n" ^
  String.concat "\n" (List.map func_decl_s funcs) ^ ")"

(* "Pretty printed" version of the AST, meant to generate a MicroC program
   from the AST.  These functions are only for pretty-printing (the -a flag)
   the AST and can be removed. *)

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
    Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

