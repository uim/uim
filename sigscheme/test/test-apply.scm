(load "./test/unittest.scm")

;; check apply
(assert-eq? "apply check1" #t (apply = '(1 1 1)))
(assert-eq? "apply check2" 6  (apply + `(1 2 ,(+ 1 2))))
(assert-equal? "apply check3" '(3) (apply cddr '((1 2 3))))
(assert-equal? "apply check4" #t (apply equal? '((1 2) (1 2))))
(assert-equal? "apply check5" "iue" (apply substring '("aiueo" 1 3)))

(assert-eq? "apply check6" 4  (apply (lambda (x y) (+ x y)) '(1 3)))
(assert-eq? "apply check7" 4  (apply (lambda (x y) (+ x y)) '(1 3)))
(assert-equal? "apply check8" '(1 2 3) (apply (lambda x x) '(1 2 3)))
(assert-equal? "apply check9" 1 (apply (lambda (x) x) '(1)))
(assert-equal? "apply check10" '(1) (apply (lambda x x) '(1)))

(assert-equal? "apply check11" 2 (apply (lambda x x 2) '(1)))

(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))
(assert-equal? "apply check5" "100" ((compose number->string *) 4 25))

(total-report)
