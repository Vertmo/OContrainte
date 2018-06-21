include ../../OMicroB/etc/Makefile.conf

TARGETS = sendMoreMoney.byte sendMoreMoney.c sendMoreMoney.elf sendMoreMoney.avr sendMoreMoney.hex

all: $(TARGETS)

$(TARGETS): sendMoreMoney.ml
	$(BIN)/omicrob -v sendMoreMoney.ml -mlopts lib/_build/OContrainte.cma,-I,lib/_build/src -stack-size=171 -heap-size=450

flash: sendMoreMoney.hex
	$(BIN)/omicrob -flash -sudo $<

simul: sendMoreMoney.elf
	./$< '../../OMicroB/bin/omicrob_pin_simulator' '../../OMicroB/bin/omicrob_circuit_simulator circuit.txt'

clean:
	@rm -f *.cmo *.cmi
	@rm -rf *.elf.dSYM
	@rm -f $(TARGETS)
