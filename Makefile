

all: 2018.dir 2020.dir 2021.dir 2022.dir

2018.dir:
	${MAKE} -C 2018

2020.dir:
	${MAKE} -C 2020

2021.dir:
	${MAKE} -C 2021 
	cd 2021; ./aoc_2021

2022.dir:
	${MAKE} -C 2022 run

