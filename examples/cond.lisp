((lambda (x) (cond ((eq x 0) 0)
                   ((eq x 1) 1)
                   (#t (+ x
                          (+ (- x 1)
                             (- x 2))))))
 10)
