default: all

.PHONY : compile
compile:
	ocamlc -c ast.mli
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex scanner.mll
	ocamlc -c scanner.ml
	ocamlc -c sast.mli
	ocamlc -c semantic_check.ml

#Tack on your own targets
.PHONY : all
all: clean compile 

.PHONY : clean
clean:
	rm -f parser.ml parser.mli scanner.ml *.cmo *.cmi
