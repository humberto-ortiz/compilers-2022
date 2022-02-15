# Depython compiler

This folder will hold a depython compiler.

For now, it can only compile integers, following the first example compiler in
[Lecture 3](https://course.ccs.neu.edu/cs4410/lec_let-and-stack_notes.html).

If you have a depython program like `uno.dp`:

```
1
```

Then you can produce an executable using make:

```
$ make uno.run
ocaml compiler.ml uno.dp > uno.s
nasm -f elf64 -o uno.o uno.s
gcc -g -m64 -o uno.run main.c uno.o
rm uno.s uno.o
$ ./uno.run
1
```
