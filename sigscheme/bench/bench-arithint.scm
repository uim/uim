(define *max* 20001)

(define (test x y)
  (if (= x *max*)
      x
      (test (- x (+ (* y 2) (/ x (abs y))))
	    (- y (+ (* x 2) (/ y (abs x)))))))

(write (test 1 1))
(newline)
