SRCS_ASM=$(wildcard ./*.asm)
SRCS=$(shell echo ${SRCS_ASM} | xargs -n1 | sort -V | xargs)
EXE=$(patsubst %.asm, %.exe, ${SRCS})
TXT=$(shell echo ${SRCS} | tr " " "\n" | grep -v b.asm | sed 's/a.asm/.txt/')
DEBUG=$(SRCS:.asm=.debug)
RUNS=$(SRCS:.asm=.run)
.PRECIOUS: %.o %.o_debug %.exe %a.txt %b.txt
.PHONY: %.run

$(warning echo ${TXT})

all: ${EXE}

debug: ${DEBUG}

run: ${RUNS}

%.debug: %.o_debug Makefile
	ld $< -o $@

%.exe: %.o Makefile 
	ld $< -o $@ -T linkerscript
	sstrip $@

%.debug: %.o Makefile
	ld $< -o $@

%.o: %.asm Makefile functions/*.asm
	nasm $< -felf64 -o $@

%.o: %.c Makefile
	gcc $< -c -o $@

%.o_debug: %.asm Makefile
	nasm -g $< -felf64 -o $@

clean:
	rm -rf *.o *.o_debug *.exe *.debug

%b.txt:
	../get_input.sh 2022 $* > $@
%a.txt:
	../get_input.sh 2022 $* > $@

%.run: %.exe %.txt
	./$< < $(word 2,$^)
