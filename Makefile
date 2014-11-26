OBJS = ast.cmo parser.cmo scanner.cmo statemap.cmo

statemap: $(OBJS)
	ocamlc -o statemap $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -w A -c $<

%.cmi : %.mli
	ocamlc -w A -c $<


default: all

.PHONY : compile
compile:
	ocamlc -c ast.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex scanner.mll
	ocamlc -c scanner.ml
	ocamlc -c 
	# ocamlc -c sast.mli
	# ocamlc -c semantic_check.ml

#Tack on your own targets
.PHONY : all
all: clean compile 

.PHONY : clean
clean:
	rm -f parser.ml parser.mli scanner.ml *.cmo *.cmi statemap

ast.cmo:
ast.cmx:
semantic_check.cmo:
semantic_check.cmx:
statemap.cmo: ast.cmo
statemap.cmx: ast.cmx
parser.cmo: ast.cmo parser.cmi 
parser.cmx: ast.cmx parser.cmi 
scanner.cmo: parser.cmi 
scanner.cmx: parser.cmx 
parser.cmi: ast.cmo 

