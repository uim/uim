(load "test/unittest.scm")

(assert-true "quasiquote check #1" (equal? '(1 2 3) `(1 2 3)))
(assert-true "quasiquote check #2" (equal? '(5) `(,(+ 2 3))))
(assert-true "unquote check" (equal? `(1 2 3) `(1 ,(+ 1 1) ,(+ 1 2))))
(assert-true "unquote-splicing check" (equal? `(1 2 3) `(1 ,@(cdr '(1 2)) 3)))
(assert-true "mixed check" (equal? '(a 3 c 7 8 9) `(a ,(+ 1 2) c ,@(cdr '(6 7 8 9)))))
(assert-equal? "nested quasiquote check #1"
               '(a `(b c ,() 0) 1)
               `(a `(b c ,(,@() ,@()) 0) 1))

(assert-equal? "nested quasiquote check #2"
               '(0 1)
               `(0 . ,(list 1)))

(assert-equal? "nested quasiquote check #3"
               '(0 . 1)
               `(0 . ,'1))

(assert-equal? "nested quasiquote check #4"
               '(0 quasiquote (unquote 1))
               `(0 . `,,(+ 1)))

(assert-true "vector quasiquote check #1"
	(equal?
	 '#(#(a b c d) e)
	 `#(,@() #(a ,@(list 'b 'c) d) e)))
(assert-equal? "vector quasiquote check #2" '(1 . #(2 3)) `(1 . #(,(+ 1 1) 3)))
(assert-equal? "vector quasiquote check #3"
               '(0 . #(1 2 3 4 5 6))
               `(0 . #(1 ,2 ,@(list 3 4) 5 ,6 ,@())))
(assert-equal? "vector quasiquote check #3"
               '#(a b)
               `#(,@(list 'a 'b)))

(total-report)
