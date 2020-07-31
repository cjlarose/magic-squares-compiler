# magic-squares-compiler

```
stack run -- plan 4 > plan4.txt
stack run -- compile < plan4.txt > enumerate4.ll
llc-9 -O3 < enumerate4.ll > enumerate4.s # for debugging
llc-9 -O3 -filetype=obj < enumerate4.ll > enumerate4.o
clang -o enumerate4 enumerate4.o
./enumerate4
```
