FILES=$(shell find . | grep py$ | sort -V)
RUNS=$(FILES:.py=.run)
.PRECIOUS: %.txt
.PHONY: %.run

run: ${RUNS}

%.txt:
	../get_input.sh 2020 $(*F) > $@
	cp $@ $(@D)/input

%b.run: %b.py %.txt
	cd $(<D); python $(<F)
%a.run: %a.py %.txt
	cd $(<D); python $(<F)
