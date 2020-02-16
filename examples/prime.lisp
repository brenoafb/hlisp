(define prime
  (lambda (n)
    (eq (smallest-divisor n) n)))

(define smallest-divisor
  (lambda (n)
    (smallest-divisor-helper n 2)))

(define smallest-divisor-helper
  (lambda (m n)
    (cond ((eq 0 (mod m n)) n)
          (#t (smallest-divisor-helper m (+ n 1))))))
