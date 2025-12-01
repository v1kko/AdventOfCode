SRCS_C=$(wildcard ./*.c)
SRCS=$(shell echo ${SRCS_C} | xargs -n1 | sort -V | xargs)
EXE=$(patsubst %.c, %.exe, ${SRCS})
RUNS=$(SRCS:.c=.run)
.PRECIOUS: %.exe %.txt
.PHONY: %.run


run: ${RUNS}

all: ${EXE}

debug: ${DEBUG}


%.exe: %.c Makefile 
	aarch64-unknown-linux-gnu-gcc -march=armv8.8-a+sve -Wall -Wextra -Og -g  $< -o $@

clean:
	rm -rf *.exe

%.txt:
	../get_input.sh 2024 $* > $@

%.run: %.exe %.txt
	qemu-aarch64 ./$< $*.txt
