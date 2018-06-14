FOLDERS = nqueens sendMoreMoney sudoku

compile:
	ocamlbuild -I lib/src magicSquare.byte
	for folder in $(FOLDERS); do ($(MAKE) --no-print-directory -C $$folder); done

clean:
	ocamlbuild -clean
	for folder in $(FOLDERS); do ($(MAKE) --no-print-directory -C $$folder clean); done
