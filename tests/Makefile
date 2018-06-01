SRCS = main.ml

compile:
	ocamlbuild tests.byte -package oUnit

tests: compile
	./tests -no-output-file -no-cache-filename

clean:
	@rm -rf tests.byte _build
