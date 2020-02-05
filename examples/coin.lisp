(define count-change (lambda (amount)
		       (cc amount 5)))

(define cc (lambda (amount kinds-of-coins)
	     (cond ((eq amount 0) 1)
		   ((< amount 0) 0)
		   ((eq kinds-of-coins 0) 0)
		   (#t (+ (cc amount
			      (- kinds-of-coins 1))
			  (cc (- amount
				 (first-denomination kinds-of-coins))
			      kinds-of-coins))))))

(define first-denomination (lambda (kinds-of-coins)
			     (cond ((eq kinds-of-coins 1) 1)
				   ((eq kinds-of-coins 2) 5)
				   ((eq kinds-of-coins 3) 10)
				   ((eq kinds-of-coins 4) 25)
				   ((eq kinds-of-coins 5) 50))))

(count-change 100)
