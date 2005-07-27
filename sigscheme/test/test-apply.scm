(load "./test/unittest.scm")

;; check apply
(assert-eq? "apply check1" #t (apply = '(1 1 1)))
(assert-eq? "apply check2" 6  (apply + `(1 2 ,(+ 1 2))))
(assert-eq? "apply check3" 4  (apply (lambda (x y) (+ x y)) '(1 3)))
(assert-eq? "apply check4" 4  (apply (lambda (x y) (+ x y)) '(1 3)))

(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))
(assert-equal? "apply check5" "100" ((compose number->string *) 4 25))

(total-report)
