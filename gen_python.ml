open Ast
open Sast
open Printf

let py_main =
    "if __name__ == '__main__':
        topDfa = mainDFA()
    "

let print = "print"
let def = "def"
let return = "return"


let gen_id = function
  Ident(id) -> id

let gen_sid = function
  SIdent(id,dt) -> id

let rec gen_tabs n = match n with
  1 -> "\t"
  | _ -> "\t"^gen_tabs (n-1)

let get_sident_name = function
  SIdent(id, scope) -> id


let gen_unop = function
  Neg -> "-"
| Inc -> "++"
| Dec -> "--"
| Not -> "!"

let gen_binop = function
  Add -> "+"
| Sub -> "-"
| Mult -> "*"
| Div -> "/"
| Equal -> "=="
| Neq -> "!="
| Less -> "<"
| Leq -> "<="
| Greater -> ">"
| Geq -> ">="
| Mod -> "%"
| And -> "and"
| Or -> "or"

let gen_var_type = function
  Int -> ""
| Float -> ""
| String -> ""
| Void -> ""

(* let rec gen_datatype = function
  Datatype(var_type) -> gen_var_type var_type
| Arraytype(datatype) -> "std::vector<" ^ gen_datatype datatype ^ ">"

let rec gen_plain_datatype = function
  Datatype(var_type) -> gen_plain_var_type var_type
| Arraytype(datatype) -> gen_plain_datatype datatype
*)

let gen_formal formal = match formal with
  Formal(datatype, id) -> gen_id id

let rec gen_sexpr sexpr = match sexpr with
  SIntLit(i, d) -> string_of_int i
| SFloatLit(f, d) -> string_of_float f
| SStringLit(s, d) -> "\"" ^ s ^ "\""
| SVariable(sident, d) -> sident
| SUnop(unop, sexpr, d) -> unop ^ "(" ^ gen_sexpr sexpr ^ ")"
| SBinop(sexpr1, binop, sexpr2, d) -> gen_sexpr sexpr1 ^ gen_binop binop ^
    gen_sexpr sexpr2 
| SExprAssign(sident, sexpr, d) -> sident ^ " = " ^ gen_sexpr sexpr 
| SCall(sident, sexpr_list, d) -> match get_sident_name sident with
    print -> gen_tabs tabs ^ "print" ^ print_sexpr_list sexpr_list
    | sleep -> gen_tabs tabs "time.sleep(" ^ gen_sexpr_list sexpr_list ^ ")\n"
    | concurrent -> gen_tabs tabs ^ "dfasToRun = set([" ^ gen_sexpr_list sexpr_list ^ "])\n"^ 
    gen_tabs tabs ^ "finishedDfas = set()\n" ^ 
    gen_tabs tabs ^ "while len(dfasToRun - finishedDfas):\n" ^ 
    gen_tabs (tabs+1) ^ "for dfa in (dfasToRun - finishedDfas):\n" ^
    gen_tabs (tabs+2) ^ "dfa.__class__.now()\n" ^ 
    gen_tabs (tabs+1) ^ "for dfa in (dfasToRun - finishedDfas):\n" ^
    gen_tabs (tabs+2) ^ "dfa.__class__.now = dfa.nexT\n" ^ 
    gen_tabs (tabs+2) ^ "if dfa.returnIt is not None: finishedDfas.add(dfa)\n"
    | itoa -> "str("^ gen_sexpr_list sexpr_list ^")\n"
    | _ -> let dfaname = gen_sident_name sident in 
    gen_tabs tabs ^ dfaname ^ "temp = " ^ gen_sid sident ^ "(" ^ gen_sexpr_list sexpr_list ^ ")\n" ^
    gen_tabs tabs ^ "while " ^ dfaname ^ "temp.returnIt is None:\n" ^
    gen_tabs (tabs+1) ^ dfaname ^ "temp.__class__.now()\n" ^
    gen_tabs (tabs+1) ^ dfaname ^ "temp.__class__.now = " ^ dfaname ^ "temp.nexT\n"


and gen_sstmt sstmt tabs = match sstmt with
  SBlock(sstmt_list) ->  gen_sstmt_list sstmt_list tabs
| SSExpr(sexpr) -> gen_tabs tabs ^ gen_sexpr sexpr ^ "\n"
| SReturn(sexpr) -> gen_tabs tabs ^ "self.returnIt =  " ^ gen_sexpr sexpr ^ "\n"
| SDeclaration(sdecl) -> ""
| SAssign(sident, sexpr) -> gen_tabs tabs ^ gen_sid sident ^ " = " ^
   gen_sexpr sexpr ^ "\n"
| STransition(sident, sexpr) -> gen_tabs tabs ^ "if(" ^ gen_sexpr sexpr ^ "):\n" ^
    gen_tabs (tabs+1) ^ "self.nexT = " ^ gen_sid sident ^ "\n"

(*gen_sdecl only appears within time blocks, VarDecls are ignored*)
and gen_sdecl sdecl lcl_prefix = match sdecl with
  SVarDecl(datatype, sid) -> ""
| SVarAssignDecl(datatype, sident, svalue) -> gen_svalue datatype svalue sident lcl_prefix

(*gen_svalue only appears within time blocks declartions, assume all local*)
and gen_svalue datatype svalue sident lcl_prefix = match svalue with
  SExprVal(sexpr) -> lcl_prefix ^ gen_plain_sid sident ^
    " = " ^ gen_sexpr sexpr lcl_prefix ^ ";\n"
| SArrVal(sexpr_list) -> gen_sid sident lcl_prefix ^ ".clear();\n" ^
     (gen_array_sexpr_list sexpr_list sident lcl_prefix) ^ ";\n"

(*semicolon and newline handled in gen_decl since array decl assignment is actually vector push_back*)
and gen_decl decl prefix = match decl with
  VarDecl(datatype, id) -> gen_datatype datatype ^ " " ^ prefix ^ gen_id id  ^ ";\n"
| VarAssignDecl(datatype, ident, value) -> gen_value datatype value ident prefix

and gen_value datatype value ident prefix = match value with
  ExprVal(expr) -> gen_datatype datatype ^ " " ^ prefix ^ gen_id ident ^ " = " ^ gen_expr expr prefix ^ ";\n"
| ArrVal(expr_list) -> "const " ^ gen_plain_datatype datatype ^ " " ^
    prefix_array ^ gen_id ident ^ "[] = {"^ gen_expr_list expr_list prefix ^ "};\n" ^
    gen_datatype datatype ^ prefix ^ gen_id ident ^"( " ^ prefix_array ^ gen_id ident ^ ", " ^
    prefix_array ^ gen_id ident ^ "+sizeof(" ^ prefix_array ^ gen_id ident ^
    ")/sizeof(" ^ prefix_array ^ gen_id ident ^ "[0]) );\n"

(* and gen_array_sexpr_list sexpr_list sident lcl_prefix = match sexpr_list with
 [] -> ""
| h::[] -> gen_sid sident lcl_prefix ^ ".push_back(" ^ gen_sexpr h lcl_prefix ^");\n"
| h::t -> gen_sid sident lcl_prefix ^ ".push_back(" ^ gen_sexpr h lcl_prefix
  ^ ");\n" ^ (gen_array_sexpr_list t sident lcl_prefix)

and gen_array_expr_list expr_list ident prefix = match expr_list with
 [] -> ""
| h::[] -> prefix ^ gen_id ident ^ ".push_back(" ^ gen_expr h prefix ^");\n"
| h::t -> prefix ^ gen_id ident ^ ".push_back(" ^ gen_expr h prefix
  ^ ");\n" ^ (gen_array_expr_list t ident prefix) *)

and gen_func func prefix =
  gen_datatype func.return ^ " " ^ prefix ^ gen_id func.fname ^
  "(" ^ gen_formal_list func.formals prefix ^ 
  ") {\n" ^ gen_stmt_list func.body prefix ^ "}\n"

and gen_decl_list decl_list prefix = match decl_list with
 [] -> ""
| h::[] -> gen_decl h prefix
| h::t -> gen_decl h prefix ^ gen_decl_list t prefix

and gen_func_list func_list prefix = match func_list with
 [] -> ""
| h::[] -> gen_func h prefix
| h::t -> gen_func h prefix ^ gen_func_list t prefix

and gen_formal_list formal_list = match formal_list with
 [] -> ""
| h::[] -> gen_formal h 
| h::t -> gen_formal h ^ ", " ^ gen_formal_list t 

and gen_sstmt_list sstmt_list tabs  = match sstmt_list with
 [] -> ""
| h::[] -> gen_sstmt h tabs
| h::t -> gen_sstmt h tabs ^ gen_sstmt_list t tabs

and gen_sexpr_list sexpr_list = match sexpr_list with
 [] -> ""
| h::[] -> gen_sexpr h 
| h::t -> gen_sexpr h ^ ", " ^ gen_sexpr_list t



(* let gen_time_block_header link =
  "unsigned int " ^ link ^ "_time = 0;\nstruct " ^ link ^
  "_link_ : public event_ {\n\tvirtual void set_next(" ^ link ^
  "_link_ *n){};\n};\nstd::vector<" ^ link ^ "_link_*> " ^ link ^ "_list;\n" *)

(* let rec gen_struct = function
  Time_struct(name, i, link, sstmt_list) -> "struct " ^ gen_name name ^
    " : public " ^ gen_link link ^ "_link_ {\n\tunsigned int time;\n\t" ^
    "unsigned int inc_time;\n\tstd::string name;\n\t" ^ 
    gen_name name ^ "() : inc_time(" ^
    string_of_int i ^ ") "^ ", time(" ^ string_of_int i ^
    "), name(\"" ^ gen_name name ^ 
    "\") {}\n\tunsigned int get_time() {return time;}\n\t" ^
    "unsigned int get_inc_time() {return inc_time;}\n\t" ^ 
    "void set_time(unsigned int time_) {time = time_;}\n\t" ^
    "std::string get_name() {return name;}\n\t" ^ gen_link link ^
    "_link_ *next;\n\tvoid set_next(" ^ gen_link link ^
    "_link_ *n) {next = n;};\n\t" ^ "void foo() {\n\t" ^
    gen_sstmt_list sstmt_list (gen_link link) ^ "\n\tif(next != NULL) {\n\t\t" ^
    gen_link link ^ "_time += next->get_inc_time();\n\t\t" ^
    "next->set_time(" ^ gen_link link ^ "_time);\n\t\t" ^
    "event_q.add(" ^ gen_link link ^ "_time, next);\n\t\t}\n\t}\n};"

and gen_struct_list struct_list = match struct_list with
 [] -> ""
| h::[] -> gen_struct h ^ "\n"
| h::t -> gen_struct h ^ "\n" ^ gen_struct_list t

let rec gen_time_block = function
  Time_block(link, decl_list, struct_list) ->
  gen_decl_list decl_list (gen_link link) ^
  gen_time_block_header (gen_link link) ^
  gen_struct_list struct_list 

and gen_time_block_list = function
 [] -> ""
| h::[] -> gen_time_block h
| h::t -> gen_time_block h  ^ "\n" ^ gen_time_block_list t

let gen_struct_obj = function
  Time_struct_obj(name, link) -> gen_name name ^ " " ^ gen_name name ^ "obj;\n\t" ^
    gen_link link ^ "_list.push_back(&" ^ gen_name name ^ "obj);\n\t"

let gen_init_linker = function
  Link(s) -> "for (int i = 0; i < " ^ s ^ "_list.size(); i++)\n\t" ^
    "{\n\t\tif (i != " ^ s ^ "_list.size()-1)\n\t\t\t" ^ 
    s ^ "_list[i]->set_next(" ^ s ^ "_list[i+1]);\n\t\telse\n\t\t\t" ^
    s ^ "_list[i]->set_next(NULL);\n\t}\n\t" ^
    "event_q.add(" ^ s ^ "_block_0obj.get_time(), &" ^ s ^ "_block_0obj);\n\t"

let gen_always_linker = function
  Link(s) -> "for (int i = 0; i < " ^ s ^ "_list.size(); i++)\n\t" ^
    "{\n\t\tif (i != " ^ s ^ "_list.size()-1)\n\t\t\t" ^ 
    s ^ "_list[i]->set_next(" ^ s ^ "_list[i+1]);\n\t\telse\n\t\t\t" ^
    s ^ "_list[i]->set_next(" ^ s ^ "_list[0]);\n\t}\n\t" ^
    "event_q.add(" ^ s ^ "_block_0obj.get_time(), &"^ s ^"_block_0obj);\n\t"

let rec gen_init_linker_list = function
 [] -> ""
| h::[] -> gen_init_linker h
| h::t -> gen_init_linker h ^ gen_init_linker_list t

let rec gen_always_linker_list = function
 [] -> ""
| h::[] -> gen_always_linker h
| h::t -> gen_always_linker h ^ gen_always_linker_list t

let rec gen_struct_obj_list = function
 [] -> ""
| h::[] -> gen_struct_obj h
| h::t -> gen_struct_obj h ^ gen_struct_obj_list t

let rec gen_event_q_add_list = function
[] -> ""
| h::[] -> gen_event_q_add h
| h::t -> gen_event_q_add h ^ gen_event_q_add_list t

and gen_event_q_add = function
  Time_struct_obj(name, link) -> "event_q.add("^ gen_name name ^
    "obj.get_time(), &" ^ gen_name name ^ "obj);\n\t" 

(*all arguments are lists*)
let gen_main = function
  Main(time_block_obj_l, init_link_l, always_link_l) ->
  gen_struct_obj_list time_block_obj_l ^ 
  gen_init_linker_list init_link_l ^
  gen_always_linker_list always_link_l
*)

let gen_dfascope_VarDecls sstmt_list tabs = match sstmt_list with
    [] -> ""
| h::[] -> "self." ^ gen_sstmt h tabs
| h::t -> "self." ^ gen_sstmt h tabs ^ gen_dfascope_VarDecls t tabs

let gen_sdfa_str sdfa_str =
  "class " gen_id sdfa_str.sdfaname ^ ":\n" ^
  gen_tabs 1 ^"def __init__(self, " ^ gen_formal_list sdfa_str.sformals ^ "):\n" ^
  gen_dfascope_VarDecls sdfa_str.svar_body 2 ^
  gen_node_list sdfa_str.snode_body (*TODO need to do gen_node_list*)

let gen_sdfa_decl = function
  SDfa_Decl(sdfa_str, dt) -> gen_sdfa_str sdfa_str

let gen_sdfa_decl_list sdfa_decl_list = 
  String.concat "\n" (List.map gen_sdfa_decl sdfa_decl_list)

let gen_program = function
  Prog(sdfa_decl_list) -> gen_sdfa_decl_list sdfa_decl_list ^ py_main
 
