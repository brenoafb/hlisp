# HLisp

This is an implementation of a simple Lisp in Haskell.

The implementation does not use any external libraries.

I wanted to keep the implementation as simple as possible, while enabling a small interpreter to run on it.

The error handling is very primitive, with just Maybes.

I based the implementation off of Paul Grahams ["Roots of Lisp"](http://www.paulgraham.com/rootsoflisp.html)
article.

## Compiling
The program can be compiled with `ghc Main` and run with `./Main`, which will start a REPL session.
You can optionally provide a script argument containing definitions (view examples folder).

```bash
~ ghc Main
./Main
> (+ 5 2)
7
```

```bash
~ runhaskell Main.hs [script]
```

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
