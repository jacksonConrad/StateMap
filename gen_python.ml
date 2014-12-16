open Ast
open Sast
open Printf

exception Error of string

let py_start =
"#########BEGIN AUTOGENERATED FUNCTIONS ###########

from time import sleep
import sys

_dfa_Dict = dict()

def _node_start():
    #do nothing: just exist as a function for the dfas to initially 
    #point to with `dfa._now` so that we can have correct formatting in
    #state()
    return

def state(dfa):
    return _dfa_Dict[dfa]._now.__name__[6:]

def makeStack(stacktype,string_of_stack):
    return map(stacktype,string_of_stack.replace('[','').replace(']','').split(','))

def concurrent(*dfasNArgs):
    dfas = [dfa(dfasNArgs[i*2+1]) for i,dfa in enumerate(dfasNArgs[::2])]
    finishedDfas = set()
    while len(set(dfas) - finishedDfas):
        for dfa in (set(dfas) - finishedDfas):
            dfa.__class__._now()
        for dfa in (set(dfas) - finishedDfas):
            dfa.__class__._now = dfa._next
        finishedDfas = set([dfa for dfa in dfas if dfa._returnVal is not None])
    return [str(dfa._returnVal) for dfa in dfas]

def callDfa(dfaClass, *args):
    dfaInstance = dfaClass(args)
    while dfaInstance._returnVal is None:
        dfaClass._now()
        dfaClass._now = dfaInstance._next
    return dfaInstance._returnVal

class EOS:
    def __init__(self):
        return
    def __type__(self):
        return 'EOSType'
    def __str__(self):
        return 'EOS'
    def __eq__(self,other):
        return type(self) == type(other)
    def __ne__(self,other):
        return type(self) != type(other)

########END AUTOGENERATED FUNCTIONS ##############
########BEGIN DFA DEFINITIONS       ##############

"

let py_end = 
"

#######END DFA DEFINITIONS          #############
if __name__ == '__main__':
    _main(sys.argv[1:] if len(sys.argv) else [])
"


let print = "print"
let def = "def"
let return = "return"


let gen_id = function
  Ident(id) -> id

(*WHY DO WE HAVE gen_sid and get_sident_name*)
let gen_sid = function
  SIdent(id,dt) -> id

let rec gen_tabs n = match n with
  0 -> ""
  |1 -> "\t"
  | _ -> "\t"^gen_tabs (n-1)

(*let get_sident_name = function
  SIdent(id, scope) -> if scope == DFAScope then "self." ^ gen_id id 
                        else gen_id id*)

let get_sident_name = function
    SIdent(id,scope) -> match scope with
        NodeScope -> "" ^ gen_id id
        |DFAScope -> "self." ^ gen_id id
        |StateScope -> "" ^ gen_id id

let gen_unop = function
  Neg -> "-"
| Not -> "not "

let gen_binop = function
  Add -> "+"
| Sub -> "-"
| Mult -> "*"
| Div -> "/"
| Equal -> "=="
| Neq -> "!="
| Lt -> "<"
| Leq -> "<="
| Gt -> ">"
| Geq -> ">="
| Mod -> "%"
| And -> " and "
| Or -> " or "

let gen_var_type = function
    Int -> "int"
    |Float -> "float"
    |String -> "str"
    |Eos -> "EOS().type"
    |Void -> "Void"
    |Stack -> "Stack"
let gen_formal formal = match formal with
  Formal(datatype, id) -> gen_id id

let rec gen_sexpr sexpr = match sexpr with
  SIntLit(i, d) -> string_of_int i
| SFloatLit(f, d) -> string_of_float f
| SStringLit(s, d) -> "\"" ^ s ^ "\""
| SVariable(sident, d) -> get_sident_name sident
| SUnop(unop, sexpr, d) -> gen_unop unop ^ "(" ^ gen_sexpr sexpr ^ ")"
| SBinop(sexpr1, binop, sexpr2, d) -> "(" ^ gen_sexpr sexpr1 ^ gen_binop binop ^
    gen_sexpr sexpr2 ^ ")" 
| SPeek(sident,dt) -> let stackName = get_sident_name sident in
    "(" ^ stackName ^ "[0] if len(" ^ stackName ^") else EOS())"
| SPop(sident,dt) -> let stackName = get_sident_name sident in
    "(" ^ stackName ^ ".pop(0) if len(" ^ stackName ^ ") else EOS())"
| SPush(sident,sexpr,dt) -> let stackName = get_sident_name sident in
    stackName ^ ".insert(0," ^ gen_sexpr sexpr ^ ")"
| SEosLit -> "EOS()"
| SCall(sident, sexpr_list, d) -> match gen_id (gen_sid sident) with
    "print" -> "print(" ^ gen_sexpr_list sexpr_list ^ ")"

    | "state" -> "state(" ^ gen_sexpr_list sexpr_list ^ ")"
    
    | "sleep" -> "sleep(" ^ gen_sexpr_list sexpr_list ^ "*.001)"
    
    | "itos" -> "str(" ^ gen_sexpr_list sexpr_list ^ ")"

    | "ftos" -> "str(" ^ gen_sexpr_list sexpr_list ^ ")"

    | "stof" -> "float(" ^ gen_sexpr_list sexpr_list ^ ")"

    | "stoi" -> "int(" ^ gen_sexpr_list sexpr_list ^ ")"

    | "input" -> "input(" ^ gen_sexpr_list sexpr_list ^ ")"
    
    | "concurrent" -> "concurrent(" ^ gen_concurrency_list sexpr_list ^")" 
    
    | _ -> let dfaname = get_sident_name sident in
    "callDfa(_" ^ dfaname ^ "," ^ gen_sexpr_list sexpr_list ^ ")" 

and gen_sstmt sstmt tabs = match sstmt with
  SBlock(sstmt_list) ->  gen_sstmt_list sstmt_list tabs
| SSExpr(sexpr) -> gen_tabs tabs ^ gen_sexpr sexpr ^ "\n"
| SReturn(sexpr) -> gen_tabs tabs ^ "self._returnVal =  " ^ gen_sexpr sexpr ^ "\n" ^
    gen_tabs tabs ^ "self._next = None\n"
| SDeclaration(sdecl) -> (match sdecl with
    SVarDecl(dt,sident) -> (match dt with
        Stacktype(_) -> gen_tabs tabs ^ get_sident_name sident ^ "= list()\n"
        |Datatype(_) -> gen_tabs tabs ^ get_sident_name sident ^ "= None\n"
        |Eostype(_) -> "type(EOS())")
   |SVarAssignDecl(dt,sident,SExprVal(sval)) -> gen_tabs tabs ^
                            get_sident_name sident ^ " = " ^ gen_sexpr sval ^ "\n")
| SAssign(sident, sexpr) -> gen_tabs tabs ^ get_sident_name sident ^ " = " ^
   gen_sexpr sexpr ^ "\n"
| STransition(sident, sexpr) -> gen_tabs tabs ^ "if(" ^ gen_sexpr sexpr ^ "):\n" ^
    gen_tabs (tabs+1) ^ "self._next = self._node_" ^ get_sident_name sident ^ "\n" ^
    gen_tabs (tabs+1) ^ "return\n"


(*semicolon and newline handled in gen_decl since array decl assignment is actually vector push_back*)
and gen_sdecl decl = match decl with
  SVarDecl(datatype, sident) -> "# Variable declared without assignment: " ^ get_sident_name sident ^ "\n"
| SVarAssignDecl(datatype, sident, value) -> get_sident_name sident ^ " = " ^ gen_svalue value ^ "\n"


and gen_svalue value = match value with
  SExprVal(sexpr) -> gen_sexpr sexpr

and gen_formal_list formal_list = match formal_list with
 [] -> ""
| h::[] -> gen_formal h 
| h::t -> gen_formal h ^ ", " ^ gen_formal_list t 

and gen_sstmt_list sstmt_list tabs  = match sstmt_list with
 [] -> ""
| h::[] -> gen_sstmt h tabs
| h::t -> gen_sstmt h tabs ^  gen_sstmt_list t tabs

and gen_sexpr_list sexpr_list = match sexpr_list with
 [] -> ""
| h::[] -> gen_sexpr h 
| h::t -> gen_sexpr h ^ ", " ^ gen_sexpr_list t


and gen_concurrent_dfa sexpr = match sexpr with
SCall(sident,sexpr_list,d) -> "_" ^ get_sident_name sident ^ ", [" ^
    gen_sexpr_list sexpr_list ^ "]"
| _ -> "TODO: semantically check args of calls to concurrent()"

and gen_concurrency_list sexpr_list = match sexpr_list with
 [] -> ""
| h::[] -> gen_concurrent_dfa h 
| h::t -> gen_concurrent_dfa h ^ ", " ^ gen_concurrency_list t

let rec gen_node_list snode_body = match snode_body with
  [] -> ""
  | SNode(sident,snode_block)::rst -> gen_tabs 1 ^ "def _node_" ^ gen_id (gen_sid sident) ^ "(self):\n" ^
    gen_sstmt snode_block 2 ^ gen_node_list rst
    
let rec get_type_from_datatype = function
    Datatype(t) -> t
    | Stacktype(ty) -> get_type_from_datatype ty
    | Eostype(e) -> e

let gen_formal_typeCast dt id = match dt with
    Stacktype(Stacktype(_)) -> raise(Error("Cannot have a formal of Stacks of Stacks"))
    |Stacktype(Eostype(_)) -> raise(Error("Cannot have a formal of Stacks of EOS"))
    |Stacktype(Datatype(Eos)) -> raise(Error("Cannot have a formal of Stacks of EOS"))
    |Stacktype(Datatype(Void)) -> raise(Error("Cannot have a formal of Stacks of Void"))
    |Stacktype(Datatype(vartype)) -> "makeStack(" ^ gen_var_type vartype ^ ","
    | _ -> match get_type_from_datatype dt with
        Int -> "int("
        |Float -> "float("
        |String -> "("
        |Void -> raise(Error("A formal cannot be of type Void"))
        |Eos -> raise(Error("A formal cannot be of type Eos"))
        |Stack -> raise(Error("A formal cannot be of type Stack"))


let rec gen_unpacked_formal_list sformals index tabs = match sformals with
    [] -> ""
    |Formal(dt,id)::rst -> gen_tabs tabs ^ "self." ^ gen_id id ^ 
        "= args[0][" ^ string_of_int index ^ "]\n" ^ 
        gen_unpacked_formal_list rst(index + 1) tabs

let rec gen_unpacked_main_formal_list sformals index tabs = match sformals with
    [] -> ""
    |Formal(dt,id)::rst ->
        gen_tabs tabs ^ "self." ^ gen_id id ^ "=" ^ gen_formal_typeCast dt id ^
        "args[0][" ^ string_of_int index ^ "])\n" ^ gen_unpacked_main_formal_list rst (index+1) tabs


let get_main_dfa_str name = match name with
  "main" -> gen_tabs 2 ^ "while self._returnVal is None:\n" ^ gen_tabs 3 ^
  "_main._now()\n" ^ gen_tabs 3 ^ "_main._now = self._next\n"
  | _ -> ""

let gen_sdfa_str sdfa_str =
  "class _" ^ gen_id sdfa_str.sdfaname ^ ":\n" ^
    gen_tabs 1 ^ "_now = _node_start\n" ^
    gen_tabs 1 ^ "def __init__(self,*args):\n" ^
    let protectedIndexArgs = match gen_id sdfa_str.sdfaname with
        "main" ->
            gen_tabs 2 ^ "try:\n" ^
            gen_unpacked_main_formal_list sdfa_str.sformals 0 3 ^
            gen_tabs 3 ^ "pass\n" ^
            gen_tabs 2 ^ "except IndexError:\n" ^
            gen_tabs 3 ^ "print('RuntimeError:Too few arguments provided to dfa \"main\"')\n" ^
            gen_tabs 3 ^ "sys.exit(1)\n"
        | _ -> gen_unpacked_formal_list sdfa_str.sformals 0 2
    in protectedIndexArgs ^
    gen_tabs 2 ^ "self._returnVal = None\n" ^
    gen_tabs 2 ^ "_" ^ (gen_id sdfa_str.sdfaname) ^ "._now = self._node_start\n" ^
    gen_tabs 2 ^ "self._next = None\n" ^
    gen_sstmt_list sdfa_str.svar_body 2 ^
    get_main_dfa_str (gen_id sdfa_str.sdfaname) ^ gen_tabs 2 ^ "return\n" ^ 
    gen_node_list sdfa_str.snode_body ^ "\n" ^
    "_dfa_Dict[\"" ^ gen_id sdfa_str.sdfaname ^ "\"] = _" ^gen_id sdfa_str.sdfaname ^ "\n"


let gen_sdfa_decl = function
  SDfa_Decl(sdfa_str, dt) -> gen_sdfa_str sdfa_str

let gen_sdfa_decl_list sdfa_decl_list = 
  String.concat "\n" (List.map gen_sdfa_decl sdfa_decl_list)

let gen_program = function
  Prog(sdfa_decl_list) -> py_start ^ gen_sdfa_decl_list sdfa_decl_list ^ py_end
 
