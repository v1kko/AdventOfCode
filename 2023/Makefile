SRCS_F90=$(wildcard ./*.f90)
SRCS=$(shell echo ${SRCS_F90} | xargs -n1 | grep -v aoc | sort -V | xargs)
TXTS=$(SRCS:.f90=.txt)

%.txt:
	../get_input.sh 2023 $* > $@

all: $(TXTS) aoc
	./aoc

aoc: ${SRCS} aoc_2023.f90
	gfortran aoc_2023.f90 -O3 -march=native -Wall -Wextra -std=f2018 -o aoc -cpp -Wno-do-subscript
aocdebug: ${SRCS} aoc_2023.f90
	gfortran aoc_2023.f90 -Og -march=native -Wall -Wextra -Werror -std=f2018 -o aocdebug -g -fcheck=all -cpp -Wno-do-subscript

test: ${TXTS} aoc_debug
	./aocdebug

fmlib.a: fmsave.f95 fmzm90.f95 fm.f95
	gfortran -c -O2 $^
	ar rv fmlib.a fm*.o
	rm fm*.o
	rm fmzm_*.mod
	rm fmvals.mod
