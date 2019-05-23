build: nqueens.ml
	omicrob -v nqueens.ml -mlopts lib/_build/OContrainte.cma,-I,lib/_build/src -circuitppx -device wemos-d1-mini -stack-size 600 -heap-size 7000

flash: nqueens.esp_elf
	omicrob -device wemos-d1-mini -flash -sudo $<

simul: nqueens.elf
	./$< '../../OMicroB/bin/omicrob_pin_simulator' '../../OMicroB/bin/omicrob_circuit_simulator circuit.txt'

clean:
	@rm -f *.cmo *.cmi
	@rm -rf *.elf.dSYM
	@rm -f $(TARGETS)
