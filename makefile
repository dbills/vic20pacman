#
all:
	dasm pacman.asm -DLARGEMEM -opanicman8k.prg -lpanicman8k.lst
	dasm pacman.asm -opanicman3k.prg -lpanicman3k.lst
	dasm pacman.asm -DSHADOWVIC -DLARGEMEM -opanicman.bin -lpanicman-coinop.lst
maze:
	perl mz1.pl < pacmaze.txt > maze.asm

VDIR=/cygdrive/c/Documents\ and\ Settings/dane.DYNAMIC/My\ Documents/Downloads/
copy:
	cp $(VDIR)/blarg.txt xplayer.asm
