build: sudoku.ml
	omicrob -v sudoku.ml -mlopts lib/_build/OContrainte.cma,-I,lib/_build/src -device wemos-d1-mini -stack-size 1024 -heap-size 22000

flash: sudoku.esp_elf
	omicrob -flash -sudo -device wemos-d1-mini $<

simul: sudoku.elf
	./$< '../../OMicroB/bin/omicrob_pin_simulator' '../../OMicroB/bin/omicrob_circuit_simulator circuit.txt'

clean:
	@rm -f *.cmo *.cmi
	@rm -rf *.elf.dSYM
	@rm -f $(TARGETS)
