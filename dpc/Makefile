%.run: %.o
	gcc -g -m64 -o $@ main.c $<

%.o: %.s
	nasm -f elf64 -o $@ $<

%.s: %.dp
	dune exec ./compiler.exe $< > $@
