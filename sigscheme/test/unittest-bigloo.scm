(load "./test/unittest.scm")

;*---------------------------------------------------------------------*/
;* For Bigloo Test                                                     */
;*---------------------------------------------------------------------*/
(define test assert-equal?)
(define (foo1 x)
   x)
(define (foo2 . x)
   x)
(define (foo3 x . y)
   (cons x y))
