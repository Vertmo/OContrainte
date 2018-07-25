include ../../OMicroB/etc/Makefile.conf

TARGETS = coloring.byte coloring.c coloring.elf coloring.avr coloring.hex

all: $(TARGETS)

$(TARGETS): coloring.ml
	$(BIN)/omicrob -v coloring.ml -mlopts lib/_build/OContrainte.cma,-I,lib/_build/src -stack-size 256

flash: coloring.hex
	$(BIN)/omicrob -flash -sudo $<

simul: coloring.elf
	./coloring.elf

clean:
	ocamlbuild -clean
	@rm -f *.cmo *.cmi
	@rm -rf *.elf.dSYM
	@rm -f $(TARGETS)
