(define null
  (lambda (x)
    (eq x '())))

(define and
  (lambda (x y)
    (cond (x (cond (y #t) (#t '())))
	  (#t '()))))

(define not
  (lambda (x)
    (cond (x '())
	  (#t #t))))

(define append
  (lambda (x y)
    (cond ((null x) y)
	  (#t (cons (car x) (append (cdr x) y))))))

(define pair
  (lambda (x y)
    (cond ((and (null x) (null y)) '())
	  ((and (not (atom x)) (not (atom y)))
	   (cons (list (car x) (car y))
		 (pair (cdr x) (cdr y)))))))

(define assoc
  (lambda (k a)
    (cond ((eq k (caar a))
	   (cadar a))
	  (#t (assoc k
			(cdr a))))))

(define eval
  (lambda (e a)
    (cond
     ((atom e) (assoc e a))
     ((atom (car e))
      (cond
       ((eq (car e) 'quote) (cadr e))
       ((eq (car e) 'atom) (atom (eval (cadr e) a)))
       ((eq (car e) 'eq) (eq (eval (cadr e) a)
			     (eval (caddr e) a)))
       ((eq (car e) 'car) (car (eval (cadr e) a)))
       ((eq (car e) 'cdr) (cdr (eval (cadr e) a)))
       ((eq (car e) 'cons) (cons (eval (cadr e) a)
				 (eval (caddr e) a)))
       ((eq (car e) 'cond) (evcon (cdr e) a))
       (#t (eval (cons (assoc (car e) a)
		       (cdr e))
		 a))))
     ((eq (caar e) 'label)
      (eval (cons (caddar e) (cdr e))
	    (cons (list (cadar e) (car e)) a)))
     ((eq (caar e) 'lambda)
      (eval (caddar e)
	    (append (pair (cadar e) (evlis (cdr e) a))
		    a))))))

(define evcon
  (lambda (c a)
    (cond ((eval (caar c) a)
	   (eval (cadar c) a))
	  (#t (evcon (cdr c) a)))))

(define evlis
  (lambda (m a)
    (cond ((null m) '())
	  (#t (cons (eval (car m) a)
		    (evlis (cdr m) a))))))

(define map
  (lambda (f xs)
    (cond ((null xs) xs)
          (#t (cons (f (car xs))
                    (map f (cdr xs)))))))

(define filter
  (lambda (p xs)
    (cond ((null xs) xs)
          ((p (car xs))
           (cons (car xs)
                 (filter p (cdr xs))))
          (#t (filter p (cdr xs))))))

(define foldr
  (lambda (f acc xs)
    (cond ((null xs) acc)
          (#t (f (car xs)
                 (foldr f acc (cdr xs)))))))

(define range
  (lambda (a b)
    (cond ((eq a b) ())
          (#t (cons a (range (+ a 1) b))))))

(define caar
  (lambda (x)
    (car (car x))))

(define cdar
  (lambda (x)
    (cdr (car x))))

(define cadr
  (lambda (x)
    (car (cdr x))))

(define cddr
  (lambda (x)
    (cdr (cdr x))))

(define cadar
  (lambda (x)
    (car (cdar x))))

(define caddr
  (lambda (x)
    (car (cddr x))))

(define cddar
  (lambda (x)
    (cdr (cdar x))))

(define caddar
  (lambda (x)
    (car (cddar x))))
