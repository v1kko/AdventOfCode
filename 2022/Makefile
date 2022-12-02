all: 1a.exe

debug: 1a.debug

%.debug: %.o_debug Makefile
	ld $< -o $@ 

%.exe: %.o Makefile
	ld $< -o $@ 

.PRECIOUS: %.o
%.o: %.asm Makefile
	nasm $< -felf64 -o $@

%.o_debug: %.asm Makefile
	nasm $< -felf64 -o $@

clean:
	rm -rf *.o *.exe *.debug
