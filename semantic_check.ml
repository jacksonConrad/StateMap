open Ast
open Sast

type symbol_table = {
    parent: symbol_table option;
    variables: (ident * datatype * value option) list;
}

type dfa_table = {
    dfas: (var_type * ident * formal list * sstmt list) list
} (*Jacked sstmt from Slang. Did NOT use stmt. sstmt is defined in sast*)

type translation_environment = {
    return_type: datatype;
    return_seen: bool; 
    location: string; (*Which DFA we're in*)
    node_scope: symbol_table; (*Scope of current node*)
    (*dfa_scope: symbol_table; (*Scope of current DFA*)*)
    dfa_lookup: dfa_table; (*Table of all DFAs*)
}

(* search for a function in our function table*)
let rec find_function (fun_scope: function_table) name = 
    List.find (fun (s,_,_,_) -> s=name) fun_scope.functions

let basic_math t1 t2 = match (t1, t2) with
    (Double, Int) -> (Double, true)
    | (Int, Double) -> (Double, true)
    | (Int, Int) -> (Int, true)
    | (Double, Double) -> (Int, true)
    | (_,_) -> (Int, false)

let relational_logic t1 t2 = match (t1, t2) with
    (Int,Int) -> (Int,true)
    | (Double,Double) -> (Int,true)
    | (Int,Double) -> (Int,true)
    | (Double,Int) -> (Int,true)
    | (_,_) -> (Int, false) 

let equal_logic t1 t2 = match(t1,t2) with
    (Int,Int) -> (Int,true)
    | (Double,Double) -> (Int,true)
    | (Int,Double) -> (Int,true)
    | (Double,Int) -> (Int,true)
    | (String,String) -> (Int,true)
    | (_,_) -> (Int,false) 

(*extracts the type from a datatype declaration*)
let rec get_type_from_datatype = function
    Datatype(t)->t
    | Stacktype(ty) -> get_type_from_datatype ty

let get_binop_return_value op typ1 typ2 = 
    le t1 = get_type_from_datatype typ1 and t2 = get_type_from_datatype typ2 in
    let (t, valid) = 
        match op with 
            Add -> basic_math t1 t2
            | Sub -> basic_math t1 t2
            | Mult -> basic_math t1 t2
            | Div -> basic_math t1 t2
            | Mod -> basic_math t1 t2
            | Equal -> equal_logic t1 t2 
            | Neq -> equal_logic t1 t2
            | Lt -> relational_logic t1 t2 
            | Leq -> relational_logic t1 t2
            | Gt -> relational_logic t1 t2
            | Geq -> relational_logic t1 t2
            | And -> relational_logic t1 t2
            | Or -> relational_logic t1 t2
        in (Datatype(t), valid) 

(*Extracts the type and name from a Formal declaration*)
let get_name_type_from_formal env = function
    Formal(datatype,ident) -> (ident,datatype,None)

(* Find the variable. If you find the variable:
    Create a new list with the updated variable *)
let update_variable env (name, datatype, value) = 
    let ((_,_,_), location) = 
    try (fun node_scope -> ((List.find (fun (s,_,_) -> s=name) node_scope),1)) env.node_scope.variables
        with Not_found -> try (fun node_scope -> ((List.find (fun (s,_,_) -> s=name) node_scope),2)) env.node_scope.parent.variables
            with Not_found -> raise Not_found in
    let new_envf =
    match location with 
        1 -> 
            (* update node  vars *)
            let new_vars = List.map (fun (n, t, v) -> if(n=name) then (name, datatype, value) else (n, t, v)) env.node_scope.variables in
            let new_sym_table = {parent = env.node_scope.parent; variables = new_vars;} in
            let new_env = {env with node_scope = new_sym_table} in
            new_env
        | 2 -> 
            (* update dfa vars *)
            let new_vars = List.map (fun (n, t, v) -> if(n=name) then (name, datatype, value) else (n, t, v)) env.node_scope.parent.variables in
            let new_sym_table = {parent = env.node_scope.parent.parent; variables = new_vars;} in
            let new_env = {env with node_scope = new_sym_table} in
            new_env
        | _ -> raise(Error("Undefined scope"))
    in new_envf

(*search for variable in global and local symbol tables*)
let find_variable env name =
    try List.find (fun (s,_,_) -> s=name) env.node_scope.variables
    with Not_found -> try List.find(fun (s,_,_) -> s=name) env.node_scope.parent.variables
    with Not_found -> raise Not_found

let peek env stack = function
    let (_,id,_)  = try find_variable env stack with
        Not_found ->
            raise(Error("Undeclared Stack ")) in id

let push env stack = function
    let (_,id,_)  = try find_variable env stack with
        Not_found ->
            raise(Error("Undeclared Stack ")) in id
   
let pop env stack = function
    let (_,id,_)  = try find_variable env stack with
        Not_found ->
            raise(Error("Undeclared Stack ")) in id

(*Semantic checking on expressions*)
let rec check_expr env e = match e with
    IntLit(i) ->Datatype(Int)
    | BoolLit(b) -> Datatype(Boolean)
    | FloatLit(f) -> Datatype(Float)
    | StringLit(s) -> Datatype(String)
    | Variable(v) -> 
        let (_,s_type,_) = try find_variable env v with 
            Not_found ->
                raise (Error("Undeclared Identifier " )) in s_type
    | Unop(u, e) -> 
        let t = check_expr env e in 
        (match u with
            Not -> if t = Datatype(Boolean) then t else raise (Error("Cannot negate a non-boolean value"))
            | _ -> if t = Datatype(Int) then t else if t = Datatype(Float) then t 
                        else
                            raise (Error("Cannot perform operation on " )))
    | Binop(e1, b, e2) -> 
        let t1 = check_expr env e1 and t2 = check_expr env e2 in 
        let (t, valid) = get_binop_return_value b t1 t2 in
        if valid then t else raise(Error("Incompatible types with binary
        operator"));
    | ExprAssign(id, e) -> let (_,t1,_) = (find_variable env id) and t2 =
        check_expr env e 
        in (if not (t1 = t2) then (raise (Error("Mismatch in types for assignment")))); check_expr env e
    | Call(id, e) -> try (let (fname, fret, fargs, fbody)  = find_function env.fun_scope id in
                let el_tys = List.map (fun exp -> check_expr env exp) e in
                let fn_tys = List.map (fun farg-> let (_,ty,_) = get_name_type_from_formal env farg in ty) fargs in
                if not (el_tys = fn_tys) then
                    raise (Error("Mismatching types in function call")) else
                    Datatype(fret))
            with Not_found ->
                raise (Error("Undeclared Function "))
    | Push(id, e) -> let (_,t1,_) = (find_variable env id) and t2 =
        check_expr env e 
        in (if not (t1 = t2) then (raise (Error("Mismatch in types for assignment")))); check_expr env e

let get_var_scope env name =  
    try (let (_,_,_) = List.find (fun (s,_,_) -> s=name) env.node_scope.variables in NodeScope)
              with Not_found -> try (let (_,_,_) = List.find(fun (s,_,_) -> s=name) env.node_scope.parent.variables in DFAScope)
                    with Not_found -> raise(Error("get_var_scope is failing"))

(*converts expr to sexpr*)
let rec get_sexpr env e = match e with
      IntLit(i) -> SIntLit(i, Datatype(Int))
      | DoubleLit(d) -> SDoubleLit(d,Datatype(Double))
      | StringLit(s) -> SStringLit(s,Datatype(String))
      | Variable(id) -> SVariable(SIdent(id, get_var_scope env id), check_expr env e)
      | Unop(u,ex) -> SUnop(u, get_sexpr env ex, check_expr env e)
      | Binop(e1,b,e2) -> SBinop(get_sexpr env e1,b, get_sexpr env e2,check_expr env e)
      | ExprAssign(id,ex) -> SExprAssign(SIdent(id, get_var_scope env id),
      get_sexpr env ex,check_expr env e) 
      | Call(id, ex_list) -> let s_ex_list = List.map(fun exp -> get_sexpr env
      exp) ex_list in SCall(SIdent(id,Global),s_ex_list, check_expr env e) 
      | Push(id, ex) -> SPush(SIdent(id, get_var_scope env id),
      get_sexpr env ex,check_expr env e)
      | Pop(id) -> SPop(SIdent(id, get_var_scope env id), check_expr env e)
      | Peek(id) -> SPeek(SIdent(id, get_var_scope env id), check_expr env e)



