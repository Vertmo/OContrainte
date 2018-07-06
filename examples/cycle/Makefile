include ../../OMicroB/etc/Makefile.conf

TARGETS = cycle.byte cycle.c cycle.elf cycle.avr cycle.hex

all: $(TARGETS)

$(TARGETS): cycle.ml
	$(BIN)/omicrob -v cycle.ml -mlopts lib/_build/OContrainte.cma,-I,lib/_build/src -stack-size 100 -heap-size 1600

flash: cycle.hex
	$(BIN)/omicrob -flash -sudo $<

simul: cycle.elf
	./$< '../../OMicroB/bin/omicrob_pin_simulator' '../../OMicroB/bin/omicrob_circuit_simulator circuit.txt'

clean:
	@rm -f *.cmo *.cmi
	@rm -rf *.elf.dSYM
	@rm -f $(TARGETS)
