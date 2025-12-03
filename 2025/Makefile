FILES=$(shell ls docs | grep md$ | grep -v index.md | sort -V)
RUNS=$(FILES:.md=.run)
.PRECIOUS: input/%.txt %.py docs/output/%a.txt docs/output/%b.txt
.PHONY: run %.run entangle

run: ${RUNS} ;

%.run: docs/output/%a.txt docs/output/%b.txt ;

input/%.txt:
	../get_input.sh 2025 $(*F) > $@

entangle:
	rm .entangled -rf
	entangled tangle --force

%.py: entangle ;

docs/output/%b.txt: %b.py input/%.txt FORCE
	python $^
	@echo `cat $@`
docs/output/%a.txt: %a.py input/%.txt FORCE
	python $^
	@echo `cat $@`

FORCE: ;

clean:
	rm *.py input/*.txt docs/output/*
