default: all

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



.PHONY : compile
compile:
	ocamlc -g ast.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex scanner.mll
	ocamlc -c scanner.ml
	ocamlc -g sast.mli
	ocamlc -g semantic_check.ml
	ocamlc -g gen_python.ml
	ocamlc -c compiler.ml
	ocamlc -o compiler -g scanner.cmo parser.cmo semantic_check.cmo gen_python.cmo compiler.cmo
sc:
	ocamlc -c sast.mli
	ocamlc -c semantic_check.ml

#Tack on your own targets

.PHONY : clean
clean:
	rm -f parser.ml parser.mli scanner.ml *.cmo *.cmi statemap compiler output.py a.out

.PHONY : all
all: 
	make clean 
	make compile 

test:
	make clean
	make compile
	./test_all.sh

run:
	make all
	./compiler < ./sample_programs/hello.sm

# Generaetd by:  ocamldep *.ml *.mli
ast.cmo :
ast.cmx :
compiler.cmo : semantic_check.cmo scanner.cmo parser.cmi gen_python.cmo
compiler.cmx : semantic_check.cmx scanner.cmx parser.cmx gen_python.cmx
gen_python.cmo : sast.cmi ast.cmo
gen_python.cmx : sast.cmi ast.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
semantic_check.cmo : sast.cmi ast.cmo
semantic_check.cmx : sast.cmi ast.cmx
statemap.cmo : scanner.cmo parser.cmi ast.cmo
statemap.cmx : scanner.cmx parser.cmx ast.cmx
parser.cmi : ast.cmo
sast.cmi : ast.cmo

lazy: 
	make clean && make compile && ocamlrun -b ./compiler
