(load "test/unittest.scm")

; basic define
(define val1 3)
(assert-eq? "*basic define check*" 3 val1)

; redefine
(define val1 5)
(assert-eq? "*redefine check*" 5 val1)

; define lambda
(define (what? x)
  "DEAFBEEF" x)
(assert-eq? "func define" 10 (what? 10))

(define (add x y)
  (+ x y))
(assert-eq? "func define" 10 (add 2 8))
