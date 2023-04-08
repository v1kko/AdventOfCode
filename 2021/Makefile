TXT=$(shell echo {1..25}.txt)

%.txt:
	../get_input.sh 2021 $* > $@

all: $(TXT)
	gfortran aoc_2021.f90 -O3 -march=native -Wall -Wextra -std=f2018 -o aoc_2021 
debug: $(TXT)
	gfortran aoc_2021.f90 -Og -march=native -Wall -Wextra -Werror -std=f2018 -o aoc_2021 -g -fcheck=all 
