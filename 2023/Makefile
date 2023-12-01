SRCS_F90=$(wildcard ./*.f90)
SRCS=$(shell echo ${SRCS_F90} | xargs -n1 | grep -v aoc | sort -V | xargs)
TXTS=$(SRCS:.f90=.txt)

%.txt:
	../get_input.sh 2023 $* > $@

all: $(TXTS) aoc
	./aoc

aoc: ${SRCS} aoc_2023.f90
	gfortran aoc_2023.f90 -O3 -march=native -Wall -Wextra -std=f2018 -o aoc -g -cpp
aoc_debug: ${SRCS} aoc_2023.f90
	gfortran aoc_2023.f90 -Og -march=native -Wall -Wextra -Werror -std=f2018 -o aoc_debug -g -fcheck=all -cpp

test: aoc_debug
	./aoc_debug

