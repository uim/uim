(load "test/unittest.scm")

; xcons
(assert-equal? "xcons test1" '(a b c) (xcons '(b c) 'a))

; cons*
(assert-equal? "cons* test1" '(1 2 3 . 4) (cons* 1 2 3 4))
(assert-equal? "cons* test2" 1 (cons* 1))

; make-list
(assert-equal? "make-list test1" '(c c c c) (make-list 4 'c))
(assert-equal? "make-list test2" '(1 2 3 4) (make-list 4))
(assert-equal? "make-list test2" '() (make-list 0 'c))
(assert-equal? "make-list test2" '() (make-list 0))

; list-tabulate
(assert-equal? "list-tabulate test1" '(0 1 2 3) (list-tabulate 4 (lambda (x) x)))
(assert-equal? "list-tabulate test2" '(1 2 3 4) (list-tabulate 4 (lambda (x) (+ x 1))))
(assert-equal? "list-tabulate test2" '() (list-tabulate 0 (lambda (x) (+ x 1))))

; list-copy
(assert-equal? "list-copy test1" '(1 2 3 4) (list-copy (list 1 2 3 4)))
(assert-equal? "list-copy test2" '(1 2 (3 4)) (list-copy (list 1 2 (list 3 4))))
(assert-equal? "list-copy test3" '() (list-copy '()))

;(display (circular-list '1 '2 '3 '4))

; iota
(assert-equal? "iota test1" '(0 1 2 3 4) (iota 5))
(assert-equal? "iota test2" '(1 2 3 4 5) (iota 5 1))
(assert-equal? "iota test3" '(1 2 3 4 5) (iota 5 1 1))
(assert-equal? "iota test4" '(1 3 5 7 9) (iota 5 1 2))
(assert-equal? "iota test5" '() (iota 0))
(assert-equal? "iota test6" '(-1 0 1) (iota 3 -1 1))
(assert-equal? "iota test7" '(-3 -1 1 3) (iota 4 -3 2))

(total-report)
