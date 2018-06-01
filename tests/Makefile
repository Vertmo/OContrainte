SRCS = main.ml

compile:
	ocamlbuild -I lib tests.byte -package oUnit

tests: compile
	./tests.byte -no-output-file -no-cache-filename

clean:
	@rm -rf tests.byte _build
