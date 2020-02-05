(define factorial
  (lambda (n) (cond ((eq n 0) 0)
		    ((eq n 1) 1)
		    (#t (+ (factorial (- n 1))
			   (factorial (- n 2)))))))
(define x 7)
(define y (factorial x))
(factorial y)
