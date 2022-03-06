%.run: %.o
# Remember to run with arch -x86_64
	clang -o myTool-x86_64 -mmacosx-version-min=10.15 -arch x86_64 -o $@ main.c $<

%.o: %.s
	nasm -f macho64 -o $@ $<

%.s: %.dp
	dune exec ./compiler.exe $< > $@