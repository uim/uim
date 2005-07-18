(load "test/unittest.scm")


;; check eqv?
(assert "check both #t" (eqv? #t #t))
(assert "check both #f" (eqv? #f #f))
(assert "check symbol"  (string=? (symbol->string 'obj)
				  (symbol->string 'obj)))
(assert "check num"  (eqv? 10 10))
(assert "check alphabet char" (eqv? #\a  #\a))
(assert "check hiragana char" (eqv? #\дв #\дв))

(assert-eq? "check empty list" '() '())

(define pair1 (cons 'a 'b))
(define pair2 pair1)
(assert-eq? "check cons" pair1 pair2)

(define str1 (string #\a))
(define str2 str1)
(assert-eq? "check cons" str1 str2)

(assert-eq? "check func" + +)

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
