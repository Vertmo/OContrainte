include ../../OMicroB/etc/Makefile.conf

TARGETS = sendMoreMoney.c sendMoreMoney.elf sendMoreMoney.avr sendMoreMoney.hex

all: $(TARGETS)

compile: sendMoreMoney.ml
	ocamlbuild sendMoreMoney.byte -I lib/src

$(TARGETS): compile
	$(BIN)/omicrob -v sendMoreMoney.ml -mlopt -I lib/src -stack-size 32 -heap-size 32

flash: sendMoreMoney.hex
	$(BIN)/omicrob -flash -sudo $<

simul: sendMoreMoney.elf
	./$< '../../OMicroB/bin/omicrob_pin_simulator' '../../OMicroB/bin/omicrob_circuit_simulator circuit.txt'

clean:
	ocamlbuild -clean
	@rm -f *.cmo *.cmi
	@rm -rf *.elf.dSYM
	@rm -f $(TARGETS)
