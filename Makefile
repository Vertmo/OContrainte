FOLDERS = lib tests examples

all: compile tests

compile:
	for folder in $(FOLDERS); do ($(MAKE) --no-print-directory -C $$folder compile); done

tests: compile
	$(MAKE) --no-print-directory -C tests tests

clean:
	for folder in $(FOLDERS); do ($(MAKE) --no-print-directory -C $$folder clean); done
