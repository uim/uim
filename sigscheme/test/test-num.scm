(load "test/unittest.scm")

(assert-equal? "= test" #t (= 1 1))
(assert-equal? "+ test1" 0  (+))
(assert-equal? "+ test2" 3  (+ 3))
(assert-equal? "+ test3" 3  (+ 1 2))
(assert-equal? "+ test4" 6  (+ 1 2 3))
(assert-equal? "- test1" -3 (- 3))
(assert-equal? "- test2" -1 (- 1 2))
(assert-equal? "- test3" -4 (- 1 2 3))
(assert-equal? "* test1" 1  (*))
(assert-equal? "* test2" 2  (* 2))
(assert-equal? "* test3" 24 (* 2 3 4))
(assert-equal? "/ test1" 0  (/ 1 2))
(assert-equal? "/ test2" -1 (/ -2 2))

(assert-equal? "abs test1" 7 (abs -7))
(assert-equal? "abs test2" 7 (abs 7))

(assert-equal? "quotient test1" 0  (/ 1 2))
(assert-equal? "quotient test2" -1 (/ -2 2))

(assert-equal? "modulo test1" 1 (modulo 13 4))
(assert-equal? "modulo test2" 3 (modulo -13 4))
(assert-equal? "modulo test3" -3 (modulo 13 -4))
(assert-equal? "modulo test4" -1 (modulo -13 -4))

(assert-equal? "remainder test1" 1 (remainder 13 4))
(assert-equal? "remainder test2" -1 (remainder -13 4))
(assert-equal? "remainder test3" 1 (remainder 13 -4))
(assert-equal? "remainder test4" -1 (remainder -13 -4))

(assert-equal? "number->string test1" "1" (number->string 1))
(assert-equal? "number->string test2" "10" (number->string 10))
(assert-equal? "number->string test3" "100" (number->string 100))

(assert-equal? "string->number test1" 1   (string->number "1"))
(assert-equal? "string->number test2" 10  (string->number "10"))
(assert-equal? "string->number test2" 100 (string->number "100"))

(total-report)
