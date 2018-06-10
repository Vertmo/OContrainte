include ../../OMicroB/etc/Makefile.conf

TARGETS = nqueens.byte nqueens.c nqueens.elf nqueens.avr nqueens.hex

all: $(TARGETS)

$(TARGETS): nqueens.ml
	$(BIN)/omicrob -v nqueens.ml -mlopts lib/_build/OContrainte.cma,-I,lib/_build/src

flash: nqueens.hex
	$(BIN)/omicrob -flash -sudo $<

simul: nqueens.elf
	./$< '../../OMicroB/bin/omicrob_pin_simulator' '../../OMicroB/bin/omicrob_circuit_simulator circuit.txt'

clean:
	@rm -f *.cmo *.cmi
	@rm -rf *.elf.dSYM
	@rm -f $(TARGETS)
