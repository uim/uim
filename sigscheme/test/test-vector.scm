(load "test/unittest.scm")

(define vec (vector 'a 'b 'c 'd))

(assert "vector test"  (equal? '#(a b c d) vec))
(assert "vector? test" (vector? vec))
(assert-equal? "vector-length test" 4 (vector-length vec))
(assert-equal? "vector-ref test" 'd (vector-ref vec 3))
(assert "vector-set! test" (equal? '#(1 a "aiue" #t) (begin
							(define tmpvec (vector 1 'a "aiue" #f))
							(vector-set! tmpvec 3 #t)
							tmpvec)))

(assert "vector->list test" (equal? '(a b c d) (vector->list vec)))
(assert "list->vector test" (equal? '#(a b c d) (list->vector '(a b c d))))
(assert "vector-fill! test" (equal? '#(#f #f #f #f) (begin
						      (define tmpvec (vector #t #t #t #t))
						      (vector-fill! tmpvec #f)
						      tmpvec)))

;(print (make-vector 3))
(assert "make-vector test" (equal? '#(#f #f #f) (make-vector 3 #f)))

(total-report)
