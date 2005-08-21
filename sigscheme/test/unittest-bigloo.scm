(load "./test/unittest.scm")

;*---------------------------------------------------------------------*/
;* For Bigloo Test                                                     */
;*---------------------------------------------------------------------*/
(define (test name val expected-val)
  (assert-equal? name expected-val val))
(define (foo1 x)
   x)
(define (foo2 . x)
   x)
(define (foo3 x . y)
   (cons x y))
