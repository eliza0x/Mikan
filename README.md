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
$ mikan 'succ ((\x. if (iszero x) (succ (succ zero)) zero) zero)'
succ ((λx. if (iszero (x)) (succ (succ (zero))) (zero)) (zero))
-> (λx. if (iszero (x)) (succ (succ (zero))) (zero)) (zero)
-> succ (if (iszero (zero)) (succ (succ (zero))) (zero))
-> if (iszero (zero)) (succ (succ (zero))) (zero)
-> iszero (zero)
-> succ (if (true) (succ (succ (zero))) (zero))
-> if (true) (succ (succ (zero))) (zero)
-> succ (succ (succ (zero)))
-> succ (succ (zero))
-> succ (zero)
-> zero
---
succ (succ (succ (zero)))
```

## Is it a calculator or programming langage?
NO, it is a counter for oranges. neither more nor less.


