(define prime
  (lambda (n)
    (eq n (smallest-divisor n))))

(define smallest-divisor
  (lambda (n)
    (find-divisor n 2)))

(define find-divisor
  (lambda (n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides test-divisor n) test-divisor)
	  (#t (find-divisor n (+ test-divisor 1))))))

(define divides
  (lambda (a b)
    (eq (remainder b a) 0)))

(define even
  (lambda (a)
    (divides 2 a)))

(define remainder
  (lambda (a b)
    (- a
       (* b (/ a b)))))

(define square
  (lambda (a) (* a a)))
