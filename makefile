#
all:
	dasm pacman.asm -opac.p00;dasm pacman.asm -opac.p00 -lpac.lst
maze:
	perl mz1.pl < pacmaze.txt > maze.asm

VDIR=/cygdrive/c/Documents\ and\ Settings/dane.DYNAMIC/My\ Documents/Downloads/
copy:
	cp $(VDIR)/blarg.txt xplayer.asm
