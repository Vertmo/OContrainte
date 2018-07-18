include ../../OMicroB/etc/Makefile.conf

TARGETS = coins.byte coins.c coins.elf coins.avr coins.hex

all: $(TARGETS)

$(TARGETS): coins.ml
	$(BIN)/omicrob -v coins.ml -mlopts lib/_build/OContrainte.cma,-I,lib/_build/src -stack-size 256

flash: coins.hex
	$(BIN)/omicrob -flash -sudo $<

simul: coins.elf
	./$< '../../OMicroB/bin/omicrob_pin_simulator' '../../OMicroB/bin/omicrob_circuit_simulator circuit.txt'

clean:
	ocamlbuild -clean
	@rm -f *.cmo *.cmi
	@rm -rf *.elf.dSYM
	@rm -f $(TARGETS)
