include ../../OMicroB/etc/Makefile.conf

TARGETS = abaccba.byte abaccba.c abaccba.elf abaccba.avr abaccba.hex

all: $(TARGETS)

$(TARGETS): abaccba.ml
	$(BIN)/omicrob -v abaccba.ml -mlopts lib/_build/OContrainte.cma,-I,lib/_build/src -stack-size=200 -heap-size=1750

flash: abaccba.hex
	$(BIN)/omicrob -flash -sudo $<

simul: abaccba.elf
	./abaccba.elf

clean:
	@rm -f *.cmo *.cmi
	@rm -rf *.elf.dSYM
	@rm -f $(TARGETS)
