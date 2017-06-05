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
$ mikan '(\x. (\y. x y)) iszero zero'
(λx. (λy. x (y))) (iszero) (zero)
-> (λx. (λy. x (y))) (iszero)
-> zero
-> (λy. iszero (y)) (zero)
-> iszero (zero)
-> true
---
true
```

samplecode is [here](https://github.com/eliza0x/Mikan/tree/master/samplecode).

## LICENCE
MIT
