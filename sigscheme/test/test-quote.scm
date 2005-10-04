(load "test/unittest.scm")

(assert-true "quasiquote check" (equal? '(1 2 3) `(1 2 3)))
(assert-true "unquote check" (equal? `(1 2 3) `(1 ,(+ 1 1) ,(+ 1 2))))
(assert-true "unquote-splicing check" (equal? `(1 2 3) `(1 ,@(cdr '(1 2)) 3)))
(assert-true "mixed check" (equal? '(a 3 c 7 8 9) `(a ,(+ 1 2) c ,@(cdr '(6 7 8 9)))))
(assert-true "nested quasiquote check"
	(equal?
	 '(a `(b c ,() 0) 1)
	 `(a `(b c ,(,@() ,@()) 0) 1)))

(assert-true "vector quasiquote check"
	(equal?
	 '#(#(a b c d) e)
	 `#(,@() #(a ,@(list 'b 'c) d) e)))

(total-report)
