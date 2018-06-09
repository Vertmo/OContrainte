include ../../OMicroB/etc/Makefile.conf

TARGETS = basic.c basic.elf basic.avr basic.hex

all: $(TARGETS)

compile: basic.ml
	ocamlbuild basic.byte -I lib/src

$(TARGETS): compile
	$(BIN)/omicrob -v basic.ml -mlopt -I lib/src

flash: basic.hex
	$(BIN)/omicrob -flash -sudo $<

simul: basic.elf
	./$< '../../OMicroB/bin/omicrob_pin_simulator' '../../OMicroB/bin/omicrob_circuit_simulator circuit.txt'

clean:
	ocamlbuild -clean
	@rm -f *.cmo *.cmi
	@rm -rf *.elf.dSYM
	@rm -f $(TARGETS)
