# Mikan
Domain specific language for calculate oranges.

![ORANGES](./docs/oranges.jpeg)

## How to use
```
$ stack install
```
Stack is [here](https://docs.haskellstack.org/en/stable/README/).

## Syntax
```
$ if (iszero (succ zero)) true (if true false zero)
if (iszero? succ zero) then (true) else (if (true) then (false) else (zero))
-> iszero? succ zero
-> if (false) then (true) else (if (true) then (false) else (zero))
-> if (true) then (false) else (zero)
-> false
---
false
```

## Is it a calculator or programming langage?
NO, it is a counter for oranges. neither more nor less.


