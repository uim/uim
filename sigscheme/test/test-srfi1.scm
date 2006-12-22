;;  Filename : test-srfi1.scm
;;  About    : unit test for SRFI-1
;;
;;  Copyright (C) 2005-2006 Kazuki Ohta <mover AT hct.zaq.ne.jp>
;;
;;  All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions
;;  are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;  3. Neither the name of authors nor the names of its contributors
;;     may be used to endorse or promote products derived from this software
;;     without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
;;  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(load "./test/unittest.scm")

(if (not (provided? "srfi-1"))
    (test-skip "SRFI-1 is not enabled"))

(define tn test-name)

(use srfi-1)
(use srfi-8)

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
(assert-error "null-list? test 3" (lambda () (null-list? dotted-lst)))
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
(receive (former latter)
	 (split-at '(1 2 3 4 5 6 7) 3)
	 (assert-equal? "split-at test 1" '(1 2 3) former)
	 (assert-equal? "split-at test 2" '(4 5 6 7) latter))

; split-at!
(receive (former latter)
	 (split-at! '(1 2 3 4 5 6 7) 3)
	 (assert-equal? "split-at! test 1" '(1 2 3) former)
	 (assert-equal? "split-at! test 2" '(4 5 6 7) latter))

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

; length+
(assert-false "length+ test 1" (length+ circular-lst))
(tn "length+ proper list")
(assert-equal? (tn) 0 (length+ '()))
(assert-equal? (tn) 1 (length+ '(1)))
(assert-equal? (tn) 2 (length+ '(1 2)))
(assert-equal? (tn) 3 (length+ '(1 2 3)))
(assert-equal? (tn) 4 (length+ '(1 2 3 4)))
(tn "length+ improper list")
(assert-error  (tn) (lambda () (length+ 1)))
(assert-error  (tn) (lambda () (length+ '(1 . 2))))
(assert-error  (tn) (lambda () (length+ '(1 2 . 3))))
(assert-error  (tn) (lambda () (length+ '(1 2 3 . 4))))
(assert-error  (tn) (lambda () (length+ '(1 2 3 4 . 5))))
(tn "length+ circular list")
(define lst1 '(1))
(set-cdr! lst1 lst1)
(define lst2 '(1 2))
(set-cdr! (list-tail lst2 1) lst2)
(define lst3 '(1 2 3))
(set-cdr! (list-tail lst3 2) lst3)
(define lst4 '(1 2 3 4))
(set-cdr! (list-tail lst4 3) lst4)
(assert-false (tn) (length+ lst1))
(assert-false (tn) (length+ lst2))
(assert-false (tn) (length+ lst3))
(assert-false (tn) (length+ lst4))

; concatenate
(assert-equal? "concatenate test 1" '() (concatenate '(())))
(assert-equal? "concatenate test 2" '() (concatenate '(() ())))
(assert-equal? "concatenate test 3" '() (concatenate '(() () ())))
(assert-equal? "concatenate test 4" '(a) (concatenate '((a))))
(assert-equal? "concatenate test 5" '(a b) (concatenate '((a) (b))))
(assert-equal? "concatenate test 6" '(a b c) (concatenate '((a) (b) (c))))
(assert-equal? "concatenate test 7" '(a b) (concatenate '((a b))))
(assert-equal? "concatenate test 8" '(a b c d) (concatenate '((a b) (c d))))
(assert-equal? "concatenate test 9" '(a b c d e f) (concatenate '((a b) (c d) (e f))))

(total-report)
