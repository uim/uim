(load "./test/unittest.scm")

; xcons
(assert-equal? "xcons test1" '(a b c) (xcons '(b c) 'a))

; cons*
(assert-equal? "cons* test1" '(1 2 3 . 4) (cons* 1 2 3 4))
(assert-equal? "cons* test2" 1 (cons* 1))

; make-list
(assert-equal? "make-list test1" '(c c c c) (make-list 4 'c))
(assert-equal? "make-list test2" '() (make-list 0 'c))
(assert-equal? "make-list test3" '() (make-list 0))

; list-tabulate
(assert-equal? "list-tabulate test1" '(0 1 2 3) (list-tabulate 4 (lambda (x) x)))
(assert-equal? "list-tabulate test2" '(1 2 3 4) (list-tabulate 4 (lambda (x) (+ x 1))))
(assert-equal? "list-tabulate test3" '() (list-tabulate 0 (lambda (x) (+ x 1))))

; list-copy
(assert-equal? "list-copy test1" '(1 2 3 4) (list-copy (list 1 2 3 4)))
(assert-equal? "list-copy test2" '(1 2 (3 4)) (list-copy (list 1 2 (list 3 4))))
(assert-equal? "list-copy test3" '() (list-copy '()))

; iota
(assert-equal? "iota test1" '(0 1 2 3 4) (iota 5))
(assert-equal? "iota test2" '(1 2 3 4 5) (iota 5 1))
(assert-equal? "iota test3" '(1 2 3 4 5) (iota 5 1 1))
(assert-equal? "iota test4" '(1 3 5 7 9) (iota 5 1 2))
(assert-equal? "iota test5" '() (iota 0))
(assert-equal? "iota test6" '(-1 0 1) (iota 3 -1 1))
(assert-equal? "iota test7" '(-3 -1 1 3) (iota 4 -3 2))

; list=
(assert-true  "list= test 1" (list= eq?))
(assert-true  "list= test 2" (list= eq? '(a)))
(assert-true  "list= test 3" (list= equal? '("a" "i" "u") '("a" "i" "u")))
(assert-false "list= test 4" (list= equal? '("a" "i" "u") '("a" "i" "e")))
(assert-false "list= test 5" (list= eqv? '("a" "i" "u") '("a" "i" "u")))
(assert-true  "list= test 6" (list= equal? '("a" "i" "u") '("a" "i" "u") '("a" "i" "u")))
(assert-false "list= test 7" (list= equal? '("a" "i" "u") '("a" "i" "u") '("a" "i" "e")))

(define proper-lst '(1 2 3 4 5))
(define circular-lst (circular-list 1 2 3 4 5))
(define dotted-lst '(1 2 3 4 . 5))
(define null-lst '())
; proper-list?
(assert-true  "proper-list? test 1" (proper-list? proper-lst))
(assert-false "proper-list? test 2" (proper-list? circular-lst))
(assert-false "proper-list? test 3" (proper-list? dotted-lst))
(assert-true  "proper-list? test 4" (proper-list? null-lst))
; circular-list?
(assert-false "circular-list? test 1" (circular-list? proper-lst))
(assert-true  "circular-list? test 2" (circular-list? circular-lst))
(assert-false "circular-list? test 3" (circular-list? dotted-lst))
(assert-false "circular-list? test 4" (circular-list? null-lst))
; dotted-list?
(assert-false "circular-list? test 1" (circular-list? proper-lst))
(assert-true  "circular-list? test 2" (circular-list? circular-lst))
(assert-false "circular-list? test 3" (circular-list? dotted-lst))
(assert-false "circular-list? test 4" (circular-list? null-lst))
; not-pair?
(assert-false "not-pair? test 1" (not-pair? proper-lst))
(assert-false "not-pair? test 2" (not-pair? circular-lst))
(assert-false "not-pair? test 3" (not-pair? dotted-lst))
(assert-true  "not-pair? test 4" (not-pair? null-lst))
; null-list?
(assert-false "null-list? test 1" (null-list? proper-lst))
(assert-false "null-list? test 2" (null-list? circular-lst))
(assert-false "null-list? test 3" (null-list? dotted-lst))
(assert-true  "null-list? test 4" (null-list? null-lst))

(define num-lst (iota 10 1))
; first
(assert-equal? "first test" 1 (first num-lst))
; second
(assert-equal? "second test" 2 (second num-lst))
; third
(assert-equal? "third test" 3 (third num-lst))
; fourth
(assert-equal? "fourth test" 4 (fourth num-lst))
; fifth
(assert-equal? "fifth test" 5 (fifth num-lst))
; sixth
(assert-equal? "sixth test" 6 (sixth num-lst))
; seventh
(assert-equal? "seventh test" 7 (seventh num-lst))
; eighth
(assert-equal? "eighth test" 8 (eighth num-lst))
; ninth
(assert-equal? "ninth test" 9 (ninth num-lst))
; tenth
(assert-equal? "tenth test" 10 (tenth num-lst))

; take
(assert-equal? "take test 1" '(a b) (take '(a b c d e) 2))
(assert-equal? "take test 2" '(1 2) (take '(1 2 3 . d) 2))
(assert-equal? "take test 3" '(1 2 3) (take '(1 2 3 . d) 3))

; drop
(assert-equal? "drop test 1" '(c d e) (drop '(a b c d e) 2))
(assert-equal? "drop test 2" '(3 . d) (drop '(1 2 3 . d) 2))
(assert-equal? "drop test 3" 'd (drop '(1 2 3 . d) 3))

; take-right
(assert-equal? "take-right test 1" '(d e) (take-right '(a b c d e) 2) )
(assert-equal? "take-right test 2" '(2 3 . d) (take-right '(1 2 3 . d) 2) )
(assert-equal? "take-right test 3" 'd (take-right '(1 2 3 . d) 0) )

; drop-right
(assert-equal? "drop-right test 1" '(a b c) (drop-right '(a b c d e) 2))
(assert-equal? "drop-right test 2" '(1) (drop-right '(1 2 3 . d) 2))
(assert-equal? "drop-right test 3" '(1 2 3) (drop-right '(1 2 3 . d) 0))

; take!
(assert-equal? "take! test 1" '(a b) (take! '(a b c d e) 2))
(assert-equal? "take! test 2" '(1 2) (take! '(1 2 3 . d) 2))
(assert-equal? "take! test 3" '(1 2 3) (take! '(1 2 3 . d) 3))
(assert-equal? "take! test 4" '(1 3) (take! (circular-list 1 3 5) 8))

; drop-right!
(assert-equal? "drop-right! test 1" '(a b c) (drop-right! '(a b c d e) 2))
(assert-equal? "drop-right! test 2" '(1) (drop-right! '(1 2 3 . d) 2))
(assert-equal? "drop-right! test 3" '(1 2 3) (drop-right! '(1 2 3 . d) 0))

; split-at
; TODO : fixme! current "receive" has the problem about evaluation order
;(receive (former latter)
;	 (split-at '(1 2 3 4 5 6 7) 3)
;	 (assert-equal? "split-at test 1" '(1 2 3) former)
;	 (assert-equal? "split-at test 2" '(4 5 6 7) latter))

; split-at!
; TODO : fixme! current "receive" has the problem about evaluation order
;(receive (former latter)
;	 (split-at! '(1 2 3 4 5 6 7) 3)
;	 (assert-equal? "split-at! test 1" '(1 2 3) former)
;	 (assert-equal? "split-at! test 2" '(4 5 6 7) latter))

; last
(assert-equal? "last test 1" 'a (last '(a)))
(assert-equal? "last test 2" 'b (last '(a b)))
(assert-equal? "last test 3" 'c (last '(a b c)))
(assert-equal? "last test 4" 'c (last '(a b c . d)))

; last-pair-pair
(assert-equal? "last-pair test 1" '(a) (last-pair '(a)))
(assert-equal? "last-pair test 2" '(b) (last-pair '(a b)))
(assert-equal? "last-pair test 3" '(c) (last-pair '(a b c)))
(assert-equal? "last-pair test 4" '(c . d) (last-pair '(a b c . d)))


(total-report)
