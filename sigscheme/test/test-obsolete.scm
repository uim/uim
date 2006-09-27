;;  Filename : test-obsolete.scm
;;  About    : unit tests for obsolete miscellaneous R5RS things
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

(define *test-track-progress* #f)
(define tn test-name)


(assert-equal? "basic case check1" 'case1 (case 1
					 ((1) 'case1)
					 ((2) 'case2)))

(assert-equal? "basic case check2" 'case2 (case 2
					 ((1) 'case1)
					 ((2) 'case2)))

(assert-equal? "basic case check3" #t (case (* 2 3)
			      ((2 3 4 7)   #f)
			      ((1 4 6 8 9) #t)))

(assert-equal? "basic case else"  'caseelse (case 3
					   ((1) 'case1)
					   ((2) 'case2)
					   (else
					    'caseelse)))

(define mularg-apply
  (letrec ((apply-2 apply)
	   (append-to-last
	    (lambda (lst)
	      (if (null? (cdr lst))
		  (car lst)
		  (cons (car lst) (append-to-last (cdr lst)))))))
    (lambda args
      (apply-2 (car args) (append-to-last (cdr args))))))
(assert-equal? "basic letrec test3" '((1) . 2) (mularg-apply cons '(1) '(2)))
(assert-equal? "basic letrec test4" '(1 2) (mularg-apply cons 1 '((2))))
;; SigScheme dependent behavior
(assert-error  "basic letrec test5" (lambda ()
                                      (letrec ((letrec-a 1)
                                               (letrec-b letrec-a))
                                        letrec-b)))


(assert-equal? "basic map test1" '(2 2 2) (map cadr '((1 2) (1 2) (1 2))))
(assert-equal? "basic map test2" '(2 4 6) (map + '(1 2 3) '(1 2 3)))
(assert-equal? "basic map test3" '(2 4 6) (map (lambda (x y) (+ x y))
						'(1 2 3) '(1 2 3)))

(define (callee a)
  (assert-equal? "basic map test4" '(1 2) a))

(map callee '((1 2)))


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
