(load "test/unittest.scm")

(assert-eq? "= test" #t (= 1 1))
(assert-eq? "+ test" 3  (+ 1 2))
(assert-eq? "- test" -1 (- 1 2))
(assert-eq? "* test" 2  (* 1 2))
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

(total-report)
