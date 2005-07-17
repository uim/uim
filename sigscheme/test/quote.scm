(load "test/unittest.scm")

(print '(1 2 3))
(print `(1 2 3))

;(assert-eq? "quasiquote check" '(1 2 3) `(1 2 3))
;(assert-eq? "unquote check" `(1 2 3) `(1 ,(+ 1 1) ,(+ 1 2)))
;(assert-eq? "unquote-splicing check" `(1 2 3) `(1 ,@(car '(1 2)) 3))
;(assert-eq? "mixed check" '(a 3 c 7 8 9) `(a ,(+ 1 2) c ,@(cdr '(6 7 8 9))))
