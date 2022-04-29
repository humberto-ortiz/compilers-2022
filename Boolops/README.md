# Boolean operators

In class we started to implement Boolean operations. We can lex "&&"
(`AND`) "||" (`OR`) and "!" (`NOT`) and the comparison "==" (`D_EQUAL`). We can
parse `AND` `OR` and `NOT`, but forgot to parse `D_EQUAL`. We have sucessfully compiled `Not`, including error checking.

# Assignment

1. Complete the compiler in this directory for the Boolean operators `And` and `Or`. 
2. Complete the AST, parser and compiler in this directory for the comparison operator "==".
This one is tricky, you may have to go back and read [Lecture
5](https://course.ccs.neu.edu/cs4410/lec_tagging-values_notes.html#%28part._.Defining_comparisons_over_our_representations%29)
again.

# Extra credit.

If you feel adventurous, include error checking to verify the types of
the operands like we did with `not`.

# Test programs

I have included 3 test programs, right now they all fail:

```shell
$ make and.run
dune exec ./compiler.exe and.dp > and.s
Fatal error: exception Failure("Todavia no se como")
make: *** [Makefile:8: and.s] Error 2
rm and.s

$ make or.run
dune exec ./compiler.exe or.dp > or.s
Fatal error: exception Failure("Todavia no se como")
make: *** [Makefile:8: or.s] Error 2
rm or.s

$ make same.run
dune exec ./compiler.exe same.dp > same.s
Fatal error: exception Dune__exe__Parser.MenhirBasics.Error
make: *** [Makefile:8: same.s] Error 2
rm same.s
```

When you finish your assignment they should all run.
