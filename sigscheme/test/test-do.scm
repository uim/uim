(load "test/unittest.scm")

(define (expt-do x n)
  (do ((i 0 (+ i 1))
       (y 1))
      ((= i n) y)
    (set! y (* x y))))

(assert-eq? "expt-do test" 1024 (expt-do 2 10))

(total-report)
