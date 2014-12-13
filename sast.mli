open Ast

(* added to work with arrays *)
type scope = 
    NodeScope
    | DFAScope
    | StateScope

type sident =
    SIdent of ident * scope

type sval = 
	SExprVal of  sexpr

and sexpr = 
    SIntLit of int * datatype 
    | SFloatLit of float * datatype
    | SStringLit of string * datatype
    | SVariable of sident * datatype
    | SUnop of unop * sexpr * datatype
    | SBinop of sexpr * binop * sexpr * datatype
    (* changed int to sexpr in SArrElem *)
    (* | SExprAssign of sident * sexpr * datatype *)
    | SCall of sident * sexpr list * datatype
    | SPeek of sident * datatype
    | SPop of sident * datatype
    | SPush of sident * sexpr * datatype
    | SEosLit

type sdecl =
	SVarDecl of datatype * sident (* put these inside decl_list for each timeblock *)
	(* changed sexpr to svalue *)
	| SVarAssignDecl of datatype * sident * sval (* v_assignment and put v_decl in timeblock decl_list*)

type sstmt = 
	SBlock of sstmt list
	| SSExpr of sexpr
	| SReturn of sexpr
	| SDeclaration of sdecl
	| SAssign of sident * sexpr
  | STransition of sident * sexpr 

type snode = 
  SNode of sident * sstmt 

type sdfastr = {
  sreturn: datatype;
  sdfaname : ident;
  sformals : formal list;
  svar_body : sstmt list;
  snode_body: snode list;
}

type sdfa_decl =
	SDfa_Decl of sdfastr * datatype

type sprogram = 
  Prog of sdfa_decl list
