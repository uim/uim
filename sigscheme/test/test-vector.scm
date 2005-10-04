(load "test/unittest.scm")

(define vec (vector 'a 'b 'c 'd))

(assert-true"vector test"  (equal? '#(a b c d) vec))
(assert-true"vector? test" (vector? vec))
(assert-equal? "vector-length test" 4 (vector-length vec))
(assert-equal? "vector-ref test" 'd (vector-ref vec 3))
(assert-true"vector-set! test" (equal? '#(1 a "aiue" #t) (begin
							(define tmpvec (vector 1 'a "aiue" #f))
							(vector-set! tmpvec 3 #t)
							tmpvec)))

(assert-true"vector->list test" (equal? '(a b c d) (vector->list vec)))
(assert-true"list->vector test" (equal? '#(a b c d) (list->vector '(a b c d))))
(assert-true"vector-fill! test" (equal? '#(#f #f #f #f) (begin
						      (define tmpvec (vector #t #t #t #t))
						      (vector-fill! tmpvec #f)
						      tmpvec)))

;(print (make-vector 3))
(assert-true"make-vector test" (equal? '#(#f #f #f) (make-vector 3 #f)))

(total-report)
