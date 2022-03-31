Diagram of our compiler so far.

```mermaid
flowchart TD
in[/foo.dp/]
 -->
lexer[[lexer]] <-- mll[lexer.mll]
 -->
tok[/list of tokens/]
 -->
parser.ml <-- mly[parser.mly]
 -->
expr
 -->
anfv2
 -->
aexpr
 -->
compile_aexpr
 -->
instruction list
 -->
asm_to_string
 -->
foo.s
 -->
nasm
 -->
foo.o
 -->
gcc <-- main.c
 -->
foo.run
```
