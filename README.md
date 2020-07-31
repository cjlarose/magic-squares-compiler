# magic-squares-compiler

```bash
order=4
stack run -- plan "$order" > plan"$order".txt
stack run -- compile < plan"$order".txt > enumerate"$order".ll
llc-9 -O3 < enumerate"$order".ll > enumerate"$order".s
llc-9 -O3 -filetype=obj < enumerate"$order".ll > enumerate"$order".o
clang -o enumerate"$order" enumerate"$order".o
./enumerate"$order"
```
