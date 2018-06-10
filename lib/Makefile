include ../OMicroB/etc/Makefile.conf

compile:
	CAMLLIB=$(LIB) ocamlbuild OContrainte.cma -I src

doc:
	CAMLLIB=$(LIB) ocamlbuild OContrainte.docdir/index.html -I src

clean:
	ocamlbuild -clean
