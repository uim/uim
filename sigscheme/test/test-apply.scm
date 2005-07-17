(load "test/unittest.scm")

;; check apply
(assert-eq? "apply check" #t (apply = '(1 1 1)))
(assert-eq? "apply check" 6  (apply + '(1 2 (+ 1 2))))
(assert-eq? "apply check" 4  (apply (lambda (x y) (+ x y)) '(1 3)))

(total-report)
