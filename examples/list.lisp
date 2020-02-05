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


(define inc
  (lambda (n) (+ n 1)))

(foldr (lambda (a b) (+ a b)) 0 (range 0 10))
