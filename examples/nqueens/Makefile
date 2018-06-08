include ../../OMicroB/etc/Makefile.conf

TARGETS = nqueens.c nqueens.elf nqueens.avr nqueens.hex

all: $(TARGETS)

compile: nqueens.ml
	ocamlbuild nqueens.byte -I lib/src

$(TARGETS): compile
	$(BIN)/omicrob -v nqueens.ml -mlopt -I lib/src -stack-size 32 -heap-size 32

flash: nqueens.hex
	$(BIN)/omicrob -flash -sudo $<

simul: nqueens.elf
	./$< '../../OMicroB/bin/omicrob_pin_simulator' '../../OMicroB/bin/omicrob_circuit_simulator circuit.txt'

clean:
	ocamlbuild -clean
	@rm -f *.cmo *.cmi
	@rm -rf *.elf.dSYM
	@rm -f $(TARGETS)
