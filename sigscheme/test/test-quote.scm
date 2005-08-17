(load "test/unittest.scm")

(assert "quasiquote check" (equal? '(1 2 3) `(1 2 3)))
(assert "unquote check" (equal? `(1 2 3) `(1 ,(+ 1 1) ,(+ 1 2))))
(assert "unquote-splicing check" (equal? `(1 2 3) `(1 ,@(cdr '(1 2)) 3)))
(assert "mixed check" (equal? '(a 3 c 7 8 9) `(a ,(+ 1 2) c ,@(cdr '(6 7 8 9)))))
(assert "nested quasiquote check"
	(equal?
	 '(a `(b c ,() 0) 1)
	 `(a `(b c ,(,@() ,@()) 0) 1)))

(assert "vector quasiquote check"
	(equal?
	 '#(#(a b c d) e)
	 `#(,@() #(a ,@(list 'b 'c) d) e)))

(total-report)
