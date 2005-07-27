(load "test/unittest.scm")

(assert-equal? "basic map test1" '(2 2 2) (map cadr '((1 2) (1 2) (1 2))))
(assert-equal? "basic map test2" '(2 4 6) (map + '(1 2 3) '(1 2 3)))
(assert-equal? "basic map test3" '(2 4 6) (map (lambda (x y) (+ x y))
						'(1 2 3) '(1 2 3)))

(total-report)
