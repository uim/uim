(load "test/unittest.scm")

(assert "quasiquote check" (equal? '(1 2 3) `(1 2 3)))
(assert "unquote check" (equal? `(1 2 3) `(1 ,(+ 1 1) ,(+ 1 2))))
(assert "unquote-splicing check" (equal? `(1 2 3) `(1 ,@(cdr '(1 2)) 3)))
(assert "mixed check" (equal? '(a 3 c 7 8 9) `(a ,(+ 1 2) c ,@(cdr '(6 7 8 9)))))

(total-report)
