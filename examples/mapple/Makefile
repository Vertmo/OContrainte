include ../../OMicroB/etc/Makefile.conf

TARGETS = mapple.byte mapple.c mapple.elf mapple.avr mapple.hex

all: $(TARGETS)

$(TARGETS): mapple.ml
	$(BIN)/omicrob -v mapple.ml -mlopts lib/_build/OContrainte.cma,-I,lib/_build/src -stack-size 256 -heap-size 1000

flash: mapple.hex
	$(BIN)/omicrob -flash -sudo $<

simul: mapple.elf
	./mapple.elf

clean:
	ocamlbuild -clean
	@rm -f *.cmo *.cmi
	@rm -rf *.elf.dSYM
	@rm -f $(TARGETS)
