%.run: %.o
	gcc -g -m64 -o $@ main.c $<

%.o: %.s
	nasm -f elf64 -o $@ $<

%.s: %.dp
	dune exec ./compiler.exe $< > $@

FILES=and.dp	compiler.ml  dune-project  lexer.mll  Makefile	parser.mly  same.dp \
anf.ml	dune	     front.ml	   main.c     or.dp	README.md   syntax.ml

.PHONY: handin.tar
handin.tar: $(FILES)
	tar cf $@ $(FILES)
