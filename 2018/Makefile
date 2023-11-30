SRCS_RS=$(wildcard src/problem_*.rs)
SRCS=$(shell echo ${SRCS_RS} | xargs -n1 | sort -V | xargs)
TXTS_1=$(SRCS:.rs=.txt)
TXTS=$(shell echo ${TXTS_1} | xargs -n1 | cut -d "_" -f 2 | sort -V | xargs)

all: ${TXTS}
	cargo run -r

%.txt:
	../get_input.sh 2018 $* > $@
