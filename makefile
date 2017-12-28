# xvic -remotemonitor
# telnet localhost 6510
all: labels
	dasm pacman.asm -DLARGEMEM=1 -opanicman8k.prg;dasm pacman.asm -DLARGEMEM=1 -opac.p00 -lpanicman8k.lst
#	cp panicman3k.prg pac.p00
	cp panicman8k.prg pac.p00

maze:
	perl mz1.pl < pacmaze.txt > maze.asm

chk: all
	@if [ `sum panicman3k.prg | cut -f1 -d' '` -ne 20566 ]; then echo "3k broken";exit 1;fi
	@if [ `sum panicman8k.prg | cut -f1 -d' '` -ne 15391 ]; then echo "8k broken";exit 1;fi
	@echo "no changes found"

labels1:
	awk '{printf "al %s %s\n",$$2,$$3}' < panicman8k.lst | egrep -v 'ENDM|INCLUDE|else|endif|processor|ifconst|endm' | egrep '^al [0-9A-F]{4} [A-Za-z0-9]{4,}' >> labels2.ll
