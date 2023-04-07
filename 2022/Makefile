SRCS_ASM=$(wildcard ./*.asm)
SRCS=$(shell echo ${SRCS_ASM} | xargs -n1 | sort -V | xargs)
EXE=$(patsubst %.asm, %.exe, ${SRCS})
DEBUG=$(SRCS:.asm=.debug)
RUNS=$(SRCS:.asm=.run)
.PRECIOUS: %.o %.o_debug %.exe %.txt
.PHONY: %.run


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

%.o_debug: %.asm Makefile
	nasm -g $< -felf64 -o $@

clean:
	rm -rf *.o *.o_debug *.exe *.debug

%.txt:
	../get_input.sh 2022 $* > $@

%a.run: %a.exe %.txt
	./$< < $*.txt

%b.run: %b.exe %.txt
	./$< < $*.txt
