(define make-rat (lambda (a b)
		   (cons a (cons b ()))))

(define num (lambda (rat)
	      (car rat)))

(define denom (lambda (rat)
		(cadr rat)))

(define mult-rat (lambda (r1 r2)
		   (make-rat (* (num r1) (num r2))
			     (* (denom r1) (denom r2)))))

(define r (make-rat 1 2))

(denom r)
