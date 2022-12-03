SRCS=$(wildcard *.asm)
EXE=$(SRCS:.asm=.exe)
DEBUG=$(SRCS:.asm=.debug)
RUNS=$(SRCS:.asm=.run)
.PRECIOUS: %.o %.o_debug %.exe
.PHONY: %.run

all: ${EXE}

debug: ${DEBUG}

run: ${RUNS}

%.debug: %.o_debug Makefile
	ld $< -o $@

%.exe %.debug: %.o Makefile
	ld $< -o $@

%.o: %.asm Makefile
	nasm $< -felf64 -o $@

%.o_debug: %.asm Makefile
	nasm -g $< -felf64 -o $@

clean:
	rm -rf *.o *.o_debug *.exe *.debug

%.run: %.exe 
	@echo "cat `echo $< | sed 's@^\([0-9]\+\).*@\1@'`.txt | ./$<":
	@cat `echo $< | sed 's@^\([0-9]\+\).*@\1@'`.txt | ./$<
	

