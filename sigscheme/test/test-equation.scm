(load "test/unittest.scm")

;; check eqv?
(assert "eqv? #1" (eqv? #t #t))
(assert "eqv? #2" (eqv? #f #f))
(assert "eqv? #3" (eqv? 'abc 'abc))
(assert "eqv? #3"  (string=? (symbol->string 'obj)
			     (symbol->string 'obj)))
(assert "eqv? #4" (eqv? -1 -1))
(assert "eqv? #4" (eqv? 0 0))
(assert "eqv? #5" (eqv? #\a #\a))
(assert "eqv? #5" (eqv? #\あ #\あ))

(let ((f (lambda (x) x + 1)))
  (assert "eqv? #6" (eqv? f f)))
(let ((f (lambda (x) x + 1))
      (g (lambda (x) x + 2)))
  (assert "eqv? #6" (not (eqv? f g))))
(let ((s1 "abc")
      (s2 "abc"))
  (assert "eqv? #6" (not (eqv? s1 s2))))
(assert "eqv? #6" (not (eqv? (cons 1 2) (cons 1 2))))
(assert "eqv? #6" (not (eqv? #f 'nil)))

(assert "eqv? #7" (not (eqv? #t #f)))
(assert "eqv? #7" (not (eqv? "abc" 'abc)))
(assert "eqv? #7" (not (eqv? 'ab 'ba)))
(assert "eqv? #7" (not (eqv? #\a #\b)))
(assert "eqv? #7" (not (eqv? #\あ #\い)))
(assert "eqv? #7" (not (eqv? '() '(())
			          )))

;; check eq?
(assert-equal? "eq? check empty list" '() '())

(define pair1 (cons 'a 'b))
(define pair2 pair1)
(assert-equal? "eq? check cons" pair1 pair2)

(define str1 (string #\a))
(define str2 str1)
(assert-equal? "eq? check cons" str1 str2)

(assert-equal? "eq? check func" + +)

;; check equal?
(assert "basic equal? test1" (equal? 'a 'a))
(assert "basic equal? test2" (equal? '(a) '(a)))
(assert "basic equal? test3" (equal? '(a (b) c)
				     '(a (b) c)))
(assert "basic equal? test4" (equal? "abc" "abc"))
(assert "basic equal? test5" (equal? 2 2))
(assert "basic equal? test6" (equal? (make-vector 5 'a)
				     (make-vector 5 'a)))

(total-report)
