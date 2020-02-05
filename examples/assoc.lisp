(define caar
  (lambda (x)
    (car (car x))))

(define cdar
  (lambda (x)
    (cdr (car x))))

(define cadar
  (lambda (x)
    (car (cdar x))))

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
