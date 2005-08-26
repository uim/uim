(load "test/unittest.scm")

;; TODO: add minus number comparisons
(assert-true  "= #1" (= 1 1))
(assert-false "= #2" (= 1 2))
(assert-true  "= #3" (= 1 1 1))
(assert-false "= #4" (= 1 2 1))
(assert-false "= #5" (= 1 1 2))

;; TODO: add minus number comparisons
(assert-true  "> #1"  (> 1 0))
(assert-false "> #2"  (> 1 1))
(assert-false "> #3"  (> 1 2))
(assert-false "> #4"  (> 1 0 0))
(assert-true  "> #5"  (> 1 0 -1))
(assert-true  "> #6"  (> 1 0 -1))
(assert-false "> #7"  (> 1 0 1))
(assert-false "> #8"  (> 1 1 0))
(assert-false "> #9"  (> 1 1 1))
(assert-false "> #10" (> 1 2 1))
(assert-false "> #11" (> 1 2 0))

;; TODO: add tests for <, >=, <=

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

; numbers in various radices
(assert-true "binary number test1" (= #b1111 15))
(assert-true "binary number test2" (= #b010  2))
(assert-true "binary number test3" (= #b0    0))
(assert-true "binary number test4" (= #b-1   -1))
(assert-true "binary number test5" (= #b-10  -2))
(assert-true "binary number test6" (= #b-010 -2))
(assert-true "octal number test1"  (= #o077  63))
(assert-true "octal number test2"  (= #o361  241))
(assert-true "decimal number test1" (= #d3900 3900))
(assert-true "decimal number test2" (= #d18782 18782))
(assert-true "hexadecimal test1" (= #xffff 65535))
(assert-true "hexadecimal test2" (= #x0A7b 2683))

(total-report)
