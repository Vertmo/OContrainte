FOLDERS = nqueens sendMoreMoney sudoku cycle

compile:
	for folder in $(FOLDERS); do ($(MAKE) --no-print-directory -C $$folder); done

clean:
	ocamlbuild -clean
	for folder in $(FOLDERS); do ($(MAKE) --no-print-directory -C $$folder clean); done
