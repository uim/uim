(load "test/unittest.scm")

(assert-eq? "= test" #t (= 1 1))
(assert-eq? "+ test" 3  (+ 1 2))
(assert-eq? "- test" -1 (- 1 2))
(assert-eq? "* test" 2  (* 1 2))
(assert-eq? "/ test" 0  (/ 1 2))
(assert-eq? "/ test" -1 (/ -2 2))

(total-report)
