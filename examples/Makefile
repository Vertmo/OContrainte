FOLDERS = nqueens sendMoreMoney

compile:
	ocamlbuild -I lib/src magicSquare.byte
	ocamlbuild -I lib/src sudoku.byte
	for folder in $(FOLDERS); do ($(MAKE) --no-print-directory -C $$folder compile); done

clean:
	ocamlbuild -clean
