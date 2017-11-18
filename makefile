#
all:
	dasm pacman.asm -DLARGEMEM=1 -opanicman8k.prg;dasm pacman.asm -DLARGEMEM=1 -opac.p00 -lpanicman8k.lst
	dasm pacman.asm -opanicman3k.prg;dasm pacman.asm -opac.p00 -lpanicman3k.lst
#	cp panicman3k.prg pac.p00
	cp panicman8k.prg pac.p00
maze:
	perl mz1.pl < pacmaze.txt > maze.asm

chk: all
	@if [ `sum panicman3k.prg | cut -f1 -d' '` -ne 20566 ]; then echo "3k broken";exit 1;fi
	@if [ `sum panicman8k.prg | cut -f1 -d' '` -ne 15391 ]; then echo "8k broken";exit 1;fi
	@echo "no changes found"
