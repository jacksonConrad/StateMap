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
let rec find_dfa (dfa_lookup: dfa_table) name = 
    List.find (fun (s,_,_,_) -> s=name) dfa_lookup.dfas

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
  let t1 = get_type_from_datatype typ1 and t2 = get_type_from_datatype typ2 in
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

(*search for variable in local symbol tables*)
let find_local_variable env name =
    try List.find (fun (s,_,_) -> s=name) env.var_scope.variables
    with Not_found -> raise Not_found

let peek env stack = 
    let (_,id,_)  = try find_variable env stack with
        Not_found ->
            raise(Error("Undeclared Stack ")) in id

let push env stack = 
    let (_,id,_)  = try find_variable env stack with
        Not_found ->
            raise(Error("Undeclared Stack ")) in id
   
let pop env stack = 
    let (_,id,_)  = try find_variable env stack with
        Not_found ->
            raise(Error("Undeclared Stack ")) in id

(*Semantic checking on expressions*)
let rec check_expr env e = match e with
    IntLit(i) ->Datatype(Int)
    | DoubleLit(f) -> Datatype(Double)
    | StringLit(s) -> Datatype(String)
    | Variable(v) -> 
        let (_,s_type,_) = try find_variable env v with 
            Not_found ->
                raise (Error("Undeclared Identifier " )) in s_type
    | Unop(u, e) -> 
        let t = check_expr env e in 
        (match u with
             _ -> if t = Datatype(Int) then t else if t = Datatype(Double) then t 
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
    | Call(id, e) -> try (let (fname, fret, fargs, fbody)  = find_dfa env.dfa_scope id in
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

let get_sval env = function
    ExprVal(expr) -> SExprVal(get_sexpr env expr)

(*not sure about checking type of stack. eg: Analagous to get_data_type_of_list in Slang Line 236*)

let get_datatype_from_val env = function
    ExprVal(expr) -> check_expr env expr

let get_sdecl env decl =
    try find_local_variable env v with
      Not_found -> match decl with
        VarDecl(datatype, ident) -> (SVarDecl(datatype, SIdent(ident, Local)), env)
        | VarAssignDecl(datatype, ident, value) -> 
            let sv = get_sval env value in
        (SVarAssignDecl(datatype, SIdent(ident, Local), sv), env)

      | _ -> raise(Error("Variable already declared"))
(*    if Not_found then match decl with
        VarDecl(datatype, ident) -> (SVarDecl(datatype, SIdent(ident, Local)), env)
        | VarAssignDecl(datatype, ident, value) -> 
            let sv = get_sval env value in
        (SVarAssignDecl(datatype, SIdent(ident, Local), sv), env)*)

let get_name_type_from_decl decl = match decl with
    VarDecl(datatype, ident) -> (ident, datatype)
        | VarAssignDecl(datatype,ident,value) -> (ident,datatype)

let get_name_type_val_from_decl decl = match decl with
    VarDecl(datatype, ident) -> (ident, datatype, None)
    | VarAssignDecl(datatype, ident, value) -> (ident, datatype, Some(value))

(* returns tuple (left hand id, left hand id type, right hand value type) *)
let get_name_type_from_var env = function
    VarDecl(datatype,ident) -> (ident,datatype,None)
    | VarAssignDecl(datatype,ident,value) -> (ident,datatype,Some(value))

let add_to_var_table env name t v = 
    let new_vars = (name,t, v)::env.node_scope.variables in
    let new_sym_table = {parent = env.node_scope.parent; variables = new_vars;} in
    let new_env = {env with node_scope = new_sym_table} in
    new_env

(*function that adds variables to environment's global_scope for use with main*)
let add_to_global_table env name t v = 
    let new_vars = (name,t,v)::env.node_scope.parent.variables in
    let new_sym_table = {parent=env.node_scope.parent.parent; variables = new_vars;} in
    let new_env = {env with env.node_scope.parent = new_sym_table} in
    new_env

(* check both sides of an assignment are compatible*) 
let check_assignments type1 type2 = match (type1, type2) with
    (Int, Int) -> true
    |(Float, Float) -> true
    |(Int, Float) -> true
    |(Float, Int) -> true
    |(Boolean, Boolean) -> true
    |(String, String) -> true
    |(_,_) -> false

(* checks the type of a variable in the symbol table*)
(* Changed from "check_var_type" *)
let match_var_type env v t =
    let(name,ty,value) = find_variable env v in
    if(t<>ty) then false else true

(* Checks that a function returned if it was supposed to*)
let check_final_env env =
    (if(false = env.return_seen && env.return_type <> Datatype(Void)) then
        raise (Error("Missing Return Statement")));
    true

(* Default Table and Environment Initializations *)
let empty_table_initialization = {parent=None; variables =[];}
let empty_dfa_table_initialization = {
    dfas=[
        dfa=retrun, name, formal_list var_list node_list
    (*The state() function to get states of concurrently running dfas*)
        (String, Ident("state"), [Formal(Datatype(String),Ident("dfa"))],[],[]);
    (*The built-in print function (only prints strings)*)
        (Void, Ident("print"), [Formal(Datatype(String),Ident("str"))],[],[]);
    (*The built-in sleep function*)
        (Void, Ident("sleep"), [Formal(Datatype(Int),Ident("ms"),[],[]);
    (*The built-in int-to-string conversion function*)
        (String, Ident("itos"), [Formal(Datatype(Int),Ident("int"),[],[]);
    (*The built-in concurrent string*)
        (Stack(Datatype(String)), Ident("concurrent"), formal list,[],[])
    ]}
let empty_environment = {return_type = Datatype(Void); return_seen = false;
    location="main"; node_scope.parent = empty_table_initialization; 
    var_scope=empty_table_initialization; dfa_lookup = empty_dfa_table_initialization}

let find_global_variable env name = 
    try List.find (fun (s,_,_) -> s=name) env.node_scope.parent.variables
    with Not_found -> raise Not_found

let initialize_globals (globals, env) decl = 
    let (name, ty) = get_name_type_from_decl decl in
        let ((_,dt,_),found) = try (fun f -> ((f env name),true)) find_global_variable with 
            Not_found ->
                ((name,ty,None),false) in
        let ret = if(found=false) then
            match decl with
                VarDecl(datatype,ident) ->
                    let (name,ty,_) = get_name_type_from_var env decl in
                    let new_env = add_to_global_table env name ty None in
                    (SVarDecl(datatype,SIdent(ident,Global))::globals, new_env)
                | VarAssignDecl(dt, id, value) ->
                    let t1 = get_type_from_datatype(dt) and t2 = get_type_from_datatype(get_datatype_from_val env value) in
                    if(t1=t2) then
                        let (n, t, v) = get_name_type_val_from_decl decl in
                        let new_env = add_to_global_table env n t v in
                        (SVarAssignDecl(dt,SIdent(id,Global),get_sval env value)::globals, new_env)
                    else raise (Error("Type mismatch"))
                else
                    raise (Error("Multiple declarations")) in ret


check_stmt env stmt = match stmt with
    | Block(stmt_list) ->
        let new_env=env in
        let getter(env,acc) s =
            let (st, ne) = check_stmt env s in
            (ne, st::acc) in
        let (ls,st) = List.fold_left(fun e s ->
            getter e s) (new_env,[]) stmt_list in
        let revst = List.rev st in
        (SBlock(revst),ls)
    | Expr(e) -> 
        let _ = check_expr env e in
        (SSExpr(get_sexpr env e),env)
    | Return(e) ->
        let type1=check_expr env e in
        (if not((type1=env.return_type)) then
            raise (Error("Incompatible Return Type")));
        let new_env = {env with return_seen=true} in
        (SReturn(get_sexpr env e), new_env)
    | Ast.Declaration(decl) -> 
        (* If variable is found, multiple decls error
            If variable is not found and var is assigndecl, check for type compat *)
        let (name, ty) = get_name_type_from_decl decl in
        let ((_,dt,_),found) = try (fun f -> ((f env name),true)) find_local_variable with 
            Not_found ->
                ((name,ty,None),false) in
        let ret = if(found=false) then
            match decl with
                VarDecl(_,_) ->
                    let (sdecl,_) = get_sdecl env decl in
                    let (n, t, v) = get_name_type_val_from_decl decl in
                    let new_env = add_to_var_table env n t v in
                    (SDeclaration(sdecl), new_env)
                | VarAssignDecl(dt, id, value) ->
                    let t1 = get_type_from_datatype(dt) and t2 = get_type_from_datatype(get_datatype_from_val env value) in
                    if(t1=t2) then
                        let (sdecl,_) = get_sdecl env decl in
                        let (n, t, v) = get_name_type_val_from_decl decl in
                        let new_env = add_to_var_table env n t v in
                        (SDeclaration(sdecl), new_env)
                    else raise (Error("Type mismatch"))
                else
                    raise (Error("Multiple declarations")) in ret
    | Ast.Assign(ident, expr) ->
        (* make sure 1) variable exists, 2) variable and expr have same types *)
        let (_, dt, _) = try find_variable env ident with Not_found -> raise (Error("Uninitialized variable")) in
        let t1 = get_type_from_datatype dt 
        and t2 = get_type_from_datatype(check_expr env expr) in
        if( not(t1=t2) ) then 
            raise (Error("Mismatched type assignments"));
        let sexpr = get_sexpr env expr in
        let new_env = update_variable env (ident,dt,Some((ExprVal(expr)))) in
        (SAssign(SIdent(ident, get_var_scope env ident), sexpr), new_env)
   | Transition(idState,ex) ->
       let t=get_type_from_datatype(check_expr env e) in
       (if not(t=Int) then
           raise(Error("Improper Transition Expression Datatype")));
       (STransition((get_sexpr env ex), env)

let get_sstmt_list env stmt_list = 
     List.fold_left (fun (sstmt_list,env) stmt -> 
        let (sstmt, new_env) = check_stmt env stmt in 
        (sstmt::sstmt_list, new_env)) ([],env) stmt_list

(* add a dfa to the environment*)
let add_dfa env sdfa_decl =
    let dfa_table = env.dfa_lookup in
    let old_dfas = dfa_table.dfas in
    match sdfa_decl with
        SDfa_Decl(sdfastr, datatype) ->
            let dfa_name = sdfastr.sdfaname in
            let dfa_type = get_type_from_datatype sdfastr.sreturn in
            let dfa_formals = sdfastr.sformals in
            let dfa_body = sdfastr.sbody in
            let new_dfas = (dfa_name, dfa_type, dfa_formals, dfa_body)::old_dfas in
            let new_dfa_lookup = {dfas = new_dfas} in
            let final_env = {env with dfa_lookup = new_dfa_lookup} in
            final_env

(* Semantic checking on a function*)
let check_dfa env dfa_declaration =
    let new_locals = List.fold_left(fun a vs -> (get_name_type_from_formal env vs)::a)[] dfa_declaration.formals in
    let new_node_scope = {parent=env.node_scope.parent(*Why SLANG do this:
        Some(env.node_scope)*); variables = new_locals;} in
    let new_env = {return_type = dfa_declaration.return; return_seen=false; location="in_dfa"; node_scope = new_node_scope; dfa_lookup = env.dfa_lookup} in
    (* let final_env  =List.fold_left(fun env stmt -> snd (check_stmt env stmt)) new_env func_declaration.body in *)
    let (typed_statements, final_env) = get_sstmt_list new_env dfa_declaration.body in
    let _=check_final_env final_env in
    let sdfadecl = ({sreturn = dfa_declaration.return; sdfaname =
        dfa_declaration.dfaname; sformals = dfa_declaration.formals; sbody =
            typed_statements}) in
    (SDfa_Decl(sdfadecl,dfa_declaration.return), env) 

let initialize_dfas env dfa_list = 
    let (typed_dfa,last_env) = List.fold_left
        (fun (sdfadecl_list,env) dfa-> let (sdfadecl, _) = check_dfa env dfa in 
                                       let final_env = add_dfa env sdfacdecl in                                       
                                       (sdfadecl::sdfadecl_list, final_env))
                                       ([],env) dfa_list in (typed_dfas,last_env)

(*Semantic checking on a program*)
let check_program program =
    let (dfas,(globals)) = program in
    let env = empty_environment in
    let (typed_dfas, new_env) = initialize_dfas env dfas in
    let (typed_globals, new_env2) = List.fold_left(fun (new_globals,env)
             globals -> initialize_globals (new_globals, env) globals) ([], new_env) globals in
    Prog(typed_dfas, (typed_globals))
             

