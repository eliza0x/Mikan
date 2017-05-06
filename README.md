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
$ mikan "if (iszero zero) (succ(pred(succ zero))) (succ(pred( succ zero)))"
if iszero? () then succ pred succ () else succ pred succ ()
-> iszero? ()
-> if true then succ pred succ () else succ pred succ ()
-> succ pred succ ()
-> pred succ ()
-> succ ()
-> ()
---
succ ()
```

## Is it a calculator or programming langage?
NO, it is a counter for oranges. neither more nor less.

