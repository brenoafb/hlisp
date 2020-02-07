(define max (lambda (x y)
	      (cond ((< x y) y)
		    (#t x))))

(define min (lambda (x y)
	      (cond ((< x y) x)
		    (#t y))))

(define square (lambda (x)
		 (* x x)))

(define sum-of-squares (lambda (x y)
			 (+ (square x)
			    (square y))))

(define make-rat (lambda (a b)
		   (cons a (cons b ()))))

(define num (lambda (rat)
	      (car rat)))

(define denom (lambda (rat)
		(cadr rat)))

(define mult-rat (lambda (r1 r2)
		   (make-rat (* (num r1) (num r2))
			     (* (denom r1) (denom r2)))))

(define add-rat (lambda (r1 r2)
		  (make-rat (+ (* (num r1) (denom r2))
			       (* (denom r1) (num r2)))
			    (* (denom r1) (denom r2)))))

(define factorial (lambda (n)
		    (fact-iter 1 1 n)))

(define fact-iter (lambda (product counter max-count)
		    (cond ((> counter max-count) product)
			  (#t (fact-iter (* counter product)
					 (+ counter 1)
					 max-count)))))

(factorial 10)
