
(load "./test/unittest.scm")

; basic define
(define val1 3)
(assert-eq? "basic define check" 3 val1)

; redefine
(define val1 5)
(assert-eq? "redefine check" 5 val1)

; define lambda
(define (what? x)
  "DEADBEEF" x)
(assert-eq? "func define" 10 (what? 10))

(define what2?
  (lambda (x)
    "DEADBEEF" x))
(assert-eq? "func define" 10 (what2? 10))

(define (add x y)
  (+ x y))
(assert-eq? "func define" 10 (add 2 8))

; tests for dot list arguments
(define (dotarg1 . a)
  a)
(assert-equal? "dot arg test 1" '(1 2) (dotarg1 1 2))

(define (dotarg2 a . b)
  a)
(assert-eq? "dot arg test 2" 1 (dotarg2 1 2))

(define (dotarg3 a . b)
  b)
(assert-equal? "dot arg test 3" '(2) (dotarg3 1 2))
(assert-equal? "dot arg test 4" '(2 3) (dotarg3 1 2 3))


(define (dotarg4 a b . c)
  b)
(assert-eq? "dot arg test 5" 2 (dotarg4 1 2 3))

(define (dotarg5 a b . c)
  c)
(assert-equal? "dot arg test 6" '(3 4) (dotarg5 1 2 3 4))

; test for internal define
(define (idefine-o a)
  (define (idefine-i c)
    (+ c 3))
  (idefine-i a))

(assert-eq? "internal define" 5 (idefine-o 2))

(total-report)
