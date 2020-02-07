# HLisp

This is an implementation of a simple Lisp in Haskell.
The implementation does not use any external libraries.
I wanted to keep the implementation as simple as possible, while enabling the Lisp to evaluate
itself. There is no error handling and some bugs exist.
I based the implementation off of Paul Grahams "Roots of Lisp" article.

## Examples


```
> (+ 1 2)
3
> (define x 3)
3
> (+ x 2)
5
> (eval 'x '((x 1) (y 2)))
1
> (eval '(car '(1 2 3)) '())
1
> (eval '(cdr '(1 2 3)) '())
(2 3)
```