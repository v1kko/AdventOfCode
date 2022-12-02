SRCS=$(wildcard *.asm)
EXE=$(SRCS:.asm=.exe)
DEBUG=$(SRCS:.asm=.debug)
.PRECIOUS: %.o %.o_debug

all: ${EXE}

debug: ${DEBUG}

%.debug: %.o_debug Makefile
	ld $< -o $@ 

%.exe: %.o Makefile
	ld $< -o $@ 

%.o: %.asm Makefile
	nasm $< -felf64 -o $@

%.o_debug: %.asm Makefile
	nasm $< -felf64 -o $@

clean:
	rm -rf *.o *.o_debug *.exe *.debug
