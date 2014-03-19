#
all:
	dasm pacman.asm -DLARGEMEM=1 -opanicman8k.prg;dasm pacman.asm -opa -lpanicman8k.lst
	dasm pacman.asm -opanicman3k.prg;dasm pacman.asm -opa -lpanicman3k.lst
	cp panicman3k.prg pac.p00
maze:
	perl mz1.pl < pacmaze.txt > maze.asm

VDIR=/cygdrive/c/Documents\ and\ Settings/dane.DYNAMIC/My\ Documents/Downloads/
copy:
	cp $(VDIR)/blarg.txt xplayer.asm
