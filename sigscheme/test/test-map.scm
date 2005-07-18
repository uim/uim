(load "test/unittest.scm")

(assert "basic map test1" (equal? '(2 2 2) (map cadr '((1 2) (1 2) (1 2)))))
(assert "basic map test2" (equal? '(2 4 6) (map + '(1 2 3) '(1 2 3))))
(assert "basic map test3" (equal? '(2 4 6) (map (lambda (x y) (+ x y))
						'(1 2 3) '(1 2 3))))

(total-report)
