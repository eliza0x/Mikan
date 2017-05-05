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
$ mikan "succ [[] [] []]"
succ succ succ succ []
$ mikan "pred ( succ [[] [] []] )"
succ succ succ []
$ mikan "if iszero(pred(succ [[][][]])) then true else false"
false
```

## Is it a calculator or programming langage?
NO, it is a counter for oranges. neither more nor less.

