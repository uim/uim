(load "test/unittest.scm")

(assert-eq? "= test" #t (= 1 1))
(assert-eq? "+ test1" 0  (+))
(assert-eq? "+ test2" 3  (+ 3))
(assert-eq? "+ test3" 3  (+ 1 2))
(assert-eq? "+ test4" 6  (+ 1 2 3))
(assert-eq? "- test1" -3 (- 3))
(assert-eq? "- test2" -1 (- 1 2))
(assert-eq? "- test3" -4 (- 1 2 3))
(assert-eq? "* test1" 1  (*))
(assert-eq? "* test2" 2  (* 2))
(assert-eq? "* test3" 24 (* 2 3 4))
(assert-eq? "/ test1" 0  (/ 1 2))
(assert-eq? "/ test2" -1 (/ -2 2))

(assert-eq? "abs test1" 7 (abs -7))
(assert-eq? "abs test2" 7 (abs 7))

(assert-eq? "quotient test1" 0  (/ 1 2))
(assert-eq? "quotient test2" -1 (/ -2 2))

(assert-eq? "modulo test1" 1 (modulo 13 4))
(assert-eq? "modulo test2" 3 (modulo -13 4))
(assert-eq? "modulo test3" -3 (modulo 13 -4))
(assert-eq? "modulo test4" -1 (modulo -13 -4))

(assert-eq? "remainder test1" 1 (remainder 13 4))
(assert-eq? "remainder test2" -1 (remainder -13 4))
(assert-eq? "remainder test3" 1 (remainder 13 -4))
(assert-eq? "remainder test4" -1 (remainder -13 -4))

(assert-equal? "number->string test1" "1" (number->string 1))
(assert-equal? "number->string test2" "10" (number->string 10))
(assert-equal? "number->string test3" "100" (number->string 100))

(assert-eq? "string->number test1" 1   (string->number "1"))
(assert-eq? "string->number test2" 10  (string->number "10"))
(assert-eq? "string->number test2" 100 (string->number "100"))

(total-report)
