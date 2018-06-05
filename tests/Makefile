SRCS = main.ml

compile:
	ocamlbuild -I lib/src tests.byte -package oUnit

tests: compile
	./tests.byte -no-output-file -no-cache-filename

clean:
	ocamlbuild -clean
