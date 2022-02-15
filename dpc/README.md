# Depython compiler

This folder will hold a depython compiler.

For now, it can only compile integers, following the example in Lecture 3.

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
