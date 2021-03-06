(define gt (lambda (a b)
	     (cond ((eq a b) ())
		   ((eq a 0) ())
		   ((eq b 0) #t)
		   (#t (gt (- a 1) (- b 1))))))

(define gcd (lambda (a b)
	      (cond ((eq a b) a)
		    ((gt a b) (gcd (- a b) b))
		    (#t (gcd (- b a) a)))))

(gcd 43 12)
