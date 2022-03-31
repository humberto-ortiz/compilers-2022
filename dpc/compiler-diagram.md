Diagram of our compiler so far.

```mermaid
flowchart TD
in[/foo.dp/]
 -->
lexer[[lexer]] 
 -->
tok[/list of tokens/]
 -->
parser[[parser]]
 -->
expr[/expr/]
 -->
anfv2[[anfv2]
 -->
aexpr[/aexpr/]
 -->
comp[[compile_aexpr]]
 -->
inst[/instruction list/]
 -->
asm[[asm_to_string]]
 -->
s[/foo.s/]
 -->
nasm[[nasm]
 -->
obj[/foo.o/]
 -->
gcc[[gcc]]
 -->
run[/foo.run/]

mll[lexer.mll] --> lexer
mly[parser.mly] --> parser.ml
main.c --> gcc
```
