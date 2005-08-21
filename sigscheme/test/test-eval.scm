(load "test/unittest.scm")

;; check eval
(assert-equal? "eval check" 3 (eval '(+ 1 2) '()))

(assert-equal? "eval check" 3 (eval '((lambda (x y) (+ x y)) 1 2) '()))

(total-report)
