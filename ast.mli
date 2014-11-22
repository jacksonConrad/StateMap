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
    EosLit of EOS |
    Variable of ident |
    Unop of unop * expr |
    Binop of expr * binop * expr |
    ExprAssign of ident * expr |
    Call of ident * expr list |
    Push of expr |
    Pop of expr |
    Peek of expr

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

type dfa_decl = {
    return : datatype;
    fname: ident;
    formals : formal list;
    body : stmt list;
}

type program = 
    dfa_decl list (*TODO: Not sure what open/close parens mean...program might actually
                    be more complicated. Referencing ast.mli for Slang lang*)
