include ../../OMicroB/etc/Makefile.conf

TARGETS = sudoku.byte sudoku.c sudoku.elf sudoku.avr sudoku.hex

all: $(TARGETS)

$(TARGETS): sudoku.ml
	$(BIN)/omicrob -v sudoku.ml -mlopts lib/_build/OContrainte.cma,-I,lib/_build/src -stack-size 512 -heap-size 2048

flash: sudoku.hex
	$(BIN)/omicrob -flash -sudo $<

simul: sudoku.elf
	./$< '../../OMicroB/bin/omicrob_pin_simulator' '../../OMicroB/bin/omicrob_circuit_simulator circuit.txt'

clean:
	@rm -f *.cmo *.cmi
	@rm -rf *.elf.dSYM
	@rm -f $(TARGETS)
